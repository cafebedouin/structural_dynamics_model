% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 — Positive Entitlement Reading (State Obligation to Provide Material Conditions)
 *   domain: constitutional law / human rights / political philosophy
 *
 * SUMMARY:
 *   UDHR Article 3 ('everyone has the right to life, liberty and security of
 *   person') is a contested kernel with three declared readings; this story
 *   instantiates exactly one of them, the positive_entitlement_reading, as a
 *   clean epsilon-invariant constraint. On this reading the article obligates
 *   state provision of the material conditions — welfare income, healthcare,
 *   housing — necessary for life and security. Instantiated as constitutional
 *   and statutory practice (postwar welfare states; justiciable social rights
 *   in Germany, South Africa, India, and much of Latin America), the reading
 *   produces a standing arrangement: compulsory fiscal transfers and service
 *   guarantees administered by states, together with speech and property
 *   regulations justified by the same life-and-security rationale. Per the
 *   epsilon-referent rule, epsilon is authored for THIS arrangement as it
 *   operates — the transfer-and-guarantee structure itself — never for the
 *   negative-liberty counterfactual a sibling reading would install. The
 *   claim/metric pair is independent by design: the constraint is CLAIMED as
 *   tangled_rope (a genuine coordination function carrying an asymmetric
 *   compulsory transfer) while the metrics are authored from observed
 *   operation, and the engine computes per-seat classifications from the
 *   structural data without reference to the claim. KEY AGENTS (by structural
 *   relationship): - low_income_households: Primary beneficiary
 *   (powerless/constrained) — receives cash transfers, housing support,
 *   coverage - uninsured_chronically_ill: Primary beneficiary
 *   (powerless/trapped) — obtains care unavailable on private terms -
 *   homeless_persons: Primary beneficiary (powerless/trapped) — target of the
 *   housing guarantee - affluent_property_holders: Primary target
 *   (powerful/arbitrage) — bears the steepest schedules, widest exit -
 *   middle_class_taxpayers: Target with dual position (moderate/constrained)
 *   — pays in, draws services back out - expression_rights_holders: Secondary
 *   target (moderate/constrained) — bears security-rationale speech
 *   restrictions - constitutional_legislatures: Agenda setter
 *   (institutional/constrained) — writes the statutes and tax schedules -
 *   constitutional_review_courts: Agenda setter (institutional/constrained) —
 *   adjudicates the obligation's scope - private_mutual_aid_traditions:
 *   Excluded actor (organized/trapped) — displaced predecessor provision -
 *   future_taxpayers: Excluded actor (powerless/trapped) — bear unfunded
 *   liabilities, not in the room - un_treaty_monitoring_bodies: Analytical
 *   observer (institutional/analytical) — compiles the comparative record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.58).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 — Positive Entitlement Reading (State Obligation to Provide Material Conditions)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional law / human rights / political philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '075bea34-2220-42fd-8dc6-cc6cb155d2ea').
narrative_ontology:cs_kernel_codification('075bea34-2220-42fd-8dc6-cc6cb155d2ea', fixed_text).
narrative_ontology:cs_authority_grounding('075bea34-2220-42fd-8dc6-cc6cb155d2ea', lineage).
narrative_ontology:cs_interpretation_layer_present('075bea34-2220-42fd-8dc6-cc6cb155d2ea').
narrative_ontology:cs_reading_relation('075bea34-2220-42fd-8dc6-cc6cb155d2ea', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('075bea34-2220-42fd-8dc6-cc6cb155d2ea', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('075bea34-2220-42fd-8dc6-cc6cb155d2ea', foundational, life_security_requires_positive_provision).
narrative_ontology:cs_axiom_status(life_security_requires_positive_provision, holdable).
narrative_ontology:cs_axiom_grounding('075bea34-2220-42fd-8dc6-cc6cb155d2ea', life_security_requires_positive_provision, deontological).
narrative_ontology:cs_axiom('075bea34-2220-42fd-8dc6-cc6cb155d2ea', secondary, state_as_primary_material_guarantor).
narrative_ontology:cs_axiom_status(state_as_primary_material_guarantor, holdable).
narrative_ontology:cs_axiom_grounding('075bea34-2220-42fd-8dc6-cc6cb155d2ea', state_as_primary_material_guarantor, instrumental).
narrative_ontology:cs_reference_frame('075bea34-2220-42fd-8dc6-cc6cb155d2ea', material_conditions_guarantee).
narrative_ontology:cs_drift_state('075bea34-2220-42fd-8dc6-cc6cb155d2ea', contemporary_welfare_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('075bea34-2220-42fd-8dc6-cc6cb155d2ea', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, low_income_households).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, uninsured_chronically_ill).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, homeless_persons).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, affluent_property_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, middle_class_taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, middle_class_taxpayers).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, positive_entitlement_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, material_preconditions_of_dignity).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, progressive_realization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and amend the welfare, health, and housing statutes that operationalize the guarantee; set benefit levels, eligibility rules, and the tax schedules that fund them. Bound by electoral cycles, constitutional review, and fiscal limits; withdrawing provision outright would strand dependent populations and invite judicial reversal, so revision happens at the margins rather than at the root.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicate the obligation's scope: whether housing policy meets a reasonableness standard, whether denial of healthcare access violates the guarantee, how far the security rationale reaches into speech regulation. Once social rights are justiciable in their order they cannot decline the question; each ruling expands or contracts the duty for everyone else.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_review_courts, agenda_setter,
    institutional, generational, constrained, national).

% Receive cash benefits, housing support, and coverage eligibility keyed to income. Household budgets and tenure depend on continued provision; relocating abroad forfeits most entitlements, and private-market substitutes are priced beyond reach. The dependence is the condition the guarantee exists to address.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, low_income_households, beneficiary,
    powerless, immediate, constrained, national).

% Need continuous treatment whose private cost exceeds lifetime earnings; on private terms they are uninsurable by construction. Under the arrangement they obtain care through public systems. Their alternative is untreated illness, so participation is not voluntary in any ordinary sense — the need itself binds them to the system.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, uninsured_chronically_ill, beneficiary,
    powerless, biographical, trapped, national).

% Are the residual population the housing guarantee targets: shelter access, emergency accommodation, and rehousing programs reach them conditionally. Exit consists of obtaining market housing, which their circumstances price out; many cycle between street, shelter, and institution.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, homeless_persons, beneficiary,
    powerless, immediate, trapped, local).

% Bear the steepest schedules: progressive income taxation, wealth and inheritance levies, and the regulatory discount the guarantee places on development and property rights. They hold the widest exit portfolio of any seat — offshore structures, foreign residency, asset relocation — and exercise it measurably, which caps how far the effective burden can ratchet.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, affluent_property_holders, payer,
    powerful, generational, arbitrage, global).

% Pay the payroll and income taxes that fund the bulk of transfers while drawing services back from the same systems: healthcare, pensions, schooling, unemployment insurance. The net position varies household by household and over the life course. Exit means emigration at the cost of career, family, and language, so most stay and contest the terms politically instead.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, middle_class_taxpayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, middle_class_taxpayers, beneficiary).

% Publishers, platforms, and individual speakers governed by hate-speech, public-order, and security regulations enacted under the same life-and-security rationale that grounds the provision duty. They bear categorical restrictions on what may be said and circulated; recourse runs through slow litigation and partial jurisdictional arbitrage.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, expression_rights_holders, payer,
    moderate, biographical, constrained, continental).

% Friendly societies, union funds, and parish charities that insured working populations before state provision absorbed the function. Compulsory schemes made their membership redundant and their scale unrecoverable; they survive at the margins. They would argue for pluralistic provision but no longer command the membership base to demonstrate it.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, private_mutual_aid_traditions, excluded,
    organized, generational, trapped, regional).

% Will service the debt and the demographic gap the current contribution schedule leaves behind. Not yet enfranchised or not yet born, they can object only through proxies; the size of their share is set entirely by seats that will not bear it.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, future_taxpayers, excluded,
    powerless, generational, trapped, national).

% Review periodic state reports and issue concluding observations on progressive realization of the material guarantees. They compile the comparative record that advocates and critics alike cite; they compel nothing directly and hold an analytical seat above the domestic contest.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, un_treaty_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, low_income_households).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves collective-action problems that voluntary markets repeatedly failed to solve at scale: adverse selection in insurance pools, communicable-disease and public-health externalities, regionally concentrated poverty and its destabilizing effects, and the absence of any private mechanism able to guarantee a subsistence floor when family, employer, and market all fail at once.
% TRANSFER_FUNCTION: Moves taxed income and property value from economically active and asset-holding households toward low-income households, patients, and tenants through cash benefits, in-kind healthcare, and housing programs; separately, it moves expressive freedom from speakers to state-defined public-order and security objectives through categorical speech regulation.
% ABSENT_VOICES: Future taxpayers who will carry the unfunded liabilities are not in the room and object only through proxy advocates. Private mutual-aid traditions displaced by compulsory schemes lost their platform when membership became redundant. Outvoted property and expression minorities participate electorally but lose persistently, and their constitutional objections are adjudicated by the same agenda-setting courts that uphold the guarantee. Dissent exists; it is outvoted and outlasted rather than unheard.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand the dependent population immediately: households losing transfer income, patients losing coverage mid-treatment, shelter systems closing. Fiscal relief for payers would arrive years later and partially, since private substitutes cannot be rebuilt at scale quickly. Political order in every affected jurisdiction would convulse; the world rearranges around the hole.
% FOUNDING_PROBLEM: Industrialization and urbanization destroyed the family, village, guild, and parish safety nets that previously caught destitution, producing mass exposure to unemployment, disease, and slum housing; the postwar settlement added the lesson that material desperation feeds political extremity. The arrangement was built to guarantee that no member of the polity falls below subsistence when every private fallback fails.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: historical demography of pre-welfare-state destitution (workhouse records, mortality tables), current ILO and OECD poverty and homelessness statistics, epidemiological studies of mortality gradients by income, and — decisively — the testimony of the arrangement's critics, who concede the underlying destitution while disputing the remedy. No element of the founding problem rests on recipient-side assertion alone.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 at interval end) because the arrangement moves a large, continuing share of national income by compulsion, with no individual-level consent mechanism and rates decoupled from any marginal service the payer personally receives. Suppression (0.58) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's arithmetic; only extractiveness is scaled. It reflects compulsory tax enforcement plus categorical speech restrictions, moderated by the existence of ordinary democratic revision channels. Theater ratio (0.30) is moderate-low: benefits are really delivered, but a growing share of activity is reporting performance — periodic state reports to treaty bodies, indicator dashboards, and compliance ceremonies that document the guarantee faster than they extend it. Accessibility_collapse (0.45) is well below mountain range: private insurance, charity, and market housing persist as partial alternatives, crowded and regulated but not eliminated. Resistance (0.58) is persistent and institutionalized — tax-limit movements, property-rights litigation, speech litigation, and recurring electoral contests over benefit generosity. The temporal series run on ONE shared grid (points 0, 15, 30, 45, 60, 75; every tracked metric authored at every point) so the engine samples aligned rows: extractiveness climbs with welfare-state buildout and never fully retrenches (austerity trims programs while healthcare and pension costs grow past them); suppression_requirement rises steeply during the enforcement-machinery buildout of the early decades and then flattens as the apparatus matures — that flattening is why the series is authored at all, since the story specifically traces enforcement-capacity formation; theater_ratio drifts slowly upward as documentation displaces delivery at the margin. The dynamics are monotonic drift, not cyclical, so no intermittent-reinforcement reading applies.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that divergence from the structural data. From the recipient seats the arrangement is a guarantee: the same structure that extracts from a taxpayer is the floor beneath a household whose alternative is destitution. From the affluent_property_holder seat it is a levy with an exit door — experienced as taking, softened by mobility. From the middle_class_taxpayer seat it is ambiguous by construction: money out through the tax schedule, services back through healthcare, pensions, and schooling, with the net position varying household by household. From the agenda-setter seats it is a mandate being administered. No authored scalar reconciles these; the per-seat computation is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: low_income_households, uninsured_chronically_ill, and homeless_persons sit near the full-beneficiary end (d near 0), with trapped exit pushing the chronically ill and homeless deepest into subsidized position. Victim declarations drive the opposite pole: affluent_property_holders derive near-full-target d, amplified by scope and verification difficulty at national scale, though their arbitrage-grade exit pulls them back toward the middle relative to a trapped payer. Expression_rights_holders derive high d from their victim declaration. Middle_class_taxpayers are the genuinely dual-positioned seat: rather than authoring a directionality_override (which keys on power atom and would smear across every moderate-power agent in the story), the dual position is carried structurally via secondary_role beneficiary, letting the derivation weigh both flows. No directionality_overrides are used: the beneficiary/victim-plus-exit derivation already produces the right qualitative ordering, and an override would add nothing the structural data does not say.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy resolution is declared: destitution, uninsurable illness, and homelessness persist in every jurisdiction, and the arrangement's justification remains the transition it has not finished making. The tangled_rope classification is what prevents both symmetric mislabels. Reading the arrangement as pure rope would erase the compulsory asymmetry — the transfer is not consented to at the individual level and falls unevenly by design. Reading it as pure snare would erase the coordination function — universal pooling solves adverse-selection and public-health problems that voluntary markets demonstrably failed to solve, and the services are actually delivered to actually-dependent populations. The engine's per-seat computation preserves both facts: coordination for the pooled, extraction for the levied, with the middle-class seat straddling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_3_reading_contest,
    'Which reading of the udhr_article_3 kernel is structurally authoritative — this positive_entitlement_reading, the negative_liberty_reading, or the procedural_hybrid_reading?',
    'Comparative constitutional practice and treaty-body jurisprudence over time: track whether domestic orders converge on justiciable social rights, revert to procedural floors, or stabilize a dual structure holding both.',
    'If the negative_liberty_reading prevails, the entire transfer structure dissolves from the constraint''s victim set (only persons facing state violence remain) and epsilon collapses toward zero; if the procedural_hybrid_reading prevails, the substantive welfare obligation is stripped and this story decomposes into a thin due-process floor. If this reading prevails, the authored structure stands as written.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_3_reading_contest, conceptual, 'Committer-frame routing: this constraint is one reading of the Article 3 kernel; the disagreement is located in whether ''security of person'' imposes positive provision duties, negative abstention duties, or only procedural guarantees.').

omega_variable(
    natural_right_vs_constructed_transfer,
    'Is the material-security entitlement a pre-political human right that states merely recognize, or a constructed fiscal arrangement whose shape tracks identifiable political coalitions?',
    'Cross-national variation analysis: if benefit structures track governing-coalition turnover and fiscal capacity rather than converging on a stable invariant content, the constructed reading is supported.',
    'If constructed, the constraint''s persistence depends on active political maintenance (consistent with the authored enforcement profile); if a recognized natural right, part of the measured extraction is better read as the cost of honoring an antecedent duty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_constructed_transfer, conceptual, 'Natural-law versus constructed-status ambiguity of the entitlement core.').

omega_variable(
    demographic_fiscal_sustainability,
    'Can the current transfer level survive the projected dependency-ratio deterioration, or does effective extraction on future cohorts rise sharply as the worker-to-beneficiary ratio falls?',
    'Actuarial projection reconciliation against realized contribution rates and retirement-age reforms across the OECD over the next two decades.',
    'If unsustainable, directionality shifts intergenerationally — future_taxpayers move from excluded bystanders to primary targets, and the constraint''s victim set widens without any change in statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_fiscal_sustainability, empirical, 'Whether aging demographics convert the arrangement into escalating intergenerational transfer.').

omega_variable(
    speech_restriction_separability,
    'How tightly are the expression restrictions coupled to the material-provision core of this reading, versus being separable state-capacity expansions riding on the same security rationale?',
    'Doctrinal tracing: compare speech-restriction intensity in jurisdictions with strong social-rights codification but weak speech regulation against jurisdictions where both intensify together.',
    'If separable, the epsilon attributable to the entitlement core drops materially and the expression_rights_holders victim entry belongs to a different constraint story; if coupled, the authored victim set stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speech_restriction_separability, conceptual, 'Whether hate-speech and public-order restrictions are intrinsic to this reading or contingent riders.').

omega_variable(
    mutual_aid_crowding_out,
    'Did compulsory state provision displace private mutual aid net of coverage gains — that is, were alternatives suppressed or merely superseded by a better solution to the same problem?',
    'Historical econometrics on friendly-society and union-fund membership density before and after scheme introduction, controlling for income growth.',
    'If displacement exceeded efficiency gains, accessibility_collapse is understated and the suppression component rises; if the state scheme simply outcompeted, the current authored values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_aid_crowding_out, empirical, 'Crowding-out versus supersession of pre-existing private provision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_a3_pos_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(udhr_a3_pos_tr_t0, observed).
narrative_ontology:measurement(udhr_a3_pos_tr_t15, udhr_article_3__positive_entitlement_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(udhr_a3_pos_tr_t15, observed).
narrative_ontology:measurement(udhr_a3_pos_tr_t30, udhr_article_3__positive_entitlement_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(udhr_a3_pos_tr_t30, observed).
narrative_ontology:measurement(udhr_a3_pos_tr_t45, udhr_article_3__positive_entitlement_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement_basis(udhr_a3_pos_tr_t45, observed).
narrative_ontology:measurement(udhr_a3_pos_tr_t60, udhr_article_3__positive_entitlement_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(udhr_a3_pos_tr_t60, observed).
narrative_ontology:measurement(udhr_a3_pos_tr_t75, udhr_article_3__positive_entitlement_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement_basis(udhr_a3_pos_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(udhr_a3_pos_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(udhr_a3_pos_be_t0, observed).
narrative_ontology:measurement(udhr_a3_pos_be_t15, udhr_article_3__positive_entitlement_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(udhr_a3_pos_be_t15, observed).
narrative_ontology:measurement(udhr_a3_pos_be_t30, udhr_article_3__positive_entitlement_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(udhr_a3_pos_be_t30, observed).
narrative_ontology:measurement(udhr_a3_pos_be_t45, udhr_article_3__positive_entitlement_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement_basis(udhr_a3_pos_be_t45, observed).
narrative_ontology:measurement(udhr_a3_pos_be_t60, udhr_article_3__positive_entitlement_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(udhr_a3_pos_be_t60, observed).
narrative_ontology:measurement(udhr_a3_pos_be_t75, udhr_article_3__positive_entitlement_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(udhr_a3_pos_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_a3_pos_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(udhr_a3_pos_su_t0, observed).
narrative_ontology:measurement(udhr_a3_pos_su_t15, udhr_article_3__positive_entitlement_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(udhr_a3_pos_su_t15, observed).
narrative_ontology:measurement(udhr_a3_pos_su_t30, udhr_article_3__positive_entitlement_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(udhr_a3_pos_su_t30, observed).
narrative_ontology:measurement(udhr_a3_pos_su_t45, udhr_article_3__positive_entitlement_reading, suppression_requirement, 45, 0.56).
narrative_ontology:measurement_basis(udhr_a3_pos_su_t45, observed).
narrative_ontology:measurement(udhr_a3_pos_su_t60, udhr_article_3__positive_entitlement_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement_basis(udhr_a3_pos_su_t60, observed).
narrative_ontology:measurement(udhr_a3_pos_su_t75, udhr_article_3__positive_entitlement_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement_basis(udhr_a3_pos_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'UDHR Article 3'. The single article text covers three structurally distinct claims with different epsilon values, different beneficiary/victim sets, and different failure modes: the negative_liberty_reading (prohibition on state deprivation — negligible transfer content, victims only where states act violently), this positive_entitlement_reading (obligation to provide — large compulsory transfers, victims among property and expression rights holders), and the procedural_hybrid_reading (due-process floor — thin, largely uncontested). Genealogically the negative and procedural readings are upstream: they dominated the immediate postwar settlement and are cited as the interpretive baseline from which the positive reading expanded. Per the epsilon-invariance principle these are separate stories linked by network edges, not one constraint with a measurement parameter; the family link runs in both directions so contamination analysis can trace how codification success in one reading pressures the others' operating environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
