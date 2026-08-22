% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Principle (Historical Responsibility Reading): Binding Emissions Reductions + Loss/Damage Financing
 *   domain: international/environmental/economic
 *
 * SUMMARY:
 *   The Common But Differentiated Responsibility (CBDR) principle is a
 *   contested kernel at the center of international climate governance. This
 *   story instantiates the HISTORICAL RESPONSIBILITY READING: developed
 *   nations are bound by CBDR to make emissions reductions proportional to
 *   their cumulative historical contribution to atmospheric CO2, and to
 *   finance loss and damage adaptation in developing nations as a matter of
 *   climate justice. This reading positions developed nations (high
 *   cumulative emitters) as the target of obligation and developing nations /
 *   vulnerable populations as beneficiaries of binding commitments. The
 *   constraint is claimed as tangled_rope: genuine coordination function
 *   (allocating responsibility to enable global participation) paired with
 *   asymmetric extraction (developed nations bear both emissions reduction
 *   costs and financial transfer obligations). The sibling
 *   reading—voluntary_commitment_reading—frames CBDR as permitting nationally
 *   determined contributions with technology transfer as the primary
 *   obligation, which would exit developed nations from the victim set and
 *   reposition developing nations as co-bearers of obligation. These readings
 *   FORECLOSE each other: no single binding treaty framework can hold both;
 *   they compete for institutional lock-in.
 *
 * KEY AGENTS:
 *   - Developed nations (high cumulative emitters): institutional power, constrained exit (treaty obligation), generational time horizon — enter victim set for binding obligations
 *   - Developing nations (lower cumulative emitters): organized power, constrained exit (dependency on financing), generational time horizon — beneficiaries of binding commitments
 *   - Vulnerable populations (LDCs, SIDS, Indigenous communities): powerless, trapped exit, immediate time horizon — primary beneficiaries of adaptation financing
 *   - Future generations (beyond 2050): powerless, trapped exit, civilizational time horizon — represented through moral claims, not negotiating seats
 *   - Technical compliance regimes (UNFCCC, IPCC, Compliance Committees): institutional power, analytical exit — agenda-setters administering the constraint
 *   - Voluntary commitment reading holders (some developed nations, industry coalitions): institutional power, trapped exit (excluded by the binding framework) — excluded from this reading's architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.68).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.42).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Principle (Historical Responsibility Reading): Binding Emissions Reductions + Loss/Damage Financing").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international/environmental/economic").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, 'f42c35c4-5ff3-4bdb-92ef-f1208d83c405').
narrative_ontology:cs_kernel_codification('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', fixed_text).
narrative_ontology:cs_authority_grounding('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', extraction).
narrative_ontology:cs_interpretation_layer_present('f42c35c4-5ff3-4bdb-92ef-f1208d83c405').
narrative_ontology:cs_reading_relation('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', cbdr_principle__voluntary_commitment_reading, forecloses).
narrative_ontology:cs_axiom('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', foundational, historical_cumulative_emissions_determine_responsibility).
narrative_ontology:cs_axiom_status(historical_cumulative_emissions_determine_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', historical_cumulative_emissions_determine_responsibility, deontological).
narrative_ontology:cs_axiom('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', foundational, developed_nations_bear_proportional_mitigation_and_finance_obligation).
narrative_ontology:cs_axiom_status(developed_nations_bear_proportional_mitigation_and_finance_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', developed_nations_bear_proportional_mitigation_and_finance_obligation, deontological).
narrative_ontology:cs_axiom('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', secondary, vulnerable_nations_have_climate_justice_claim_on_developed_nations).
narrative_ontology:cs_axiom_status(vulnerable_nations_have_climate_justice_claim_on_developed_nations, holdable).
narrative_ontology:cs_axiom_grounding('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', vulnerable_nations_have_climate_justice_claim_on_developed_nations, deontological).
narrative_ontology:cs_reference_frame('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', industrial_responsibility_symmetry).
narrative_ontology:cs_drift_state('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', contemporary_2024_climate_emergency, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f42c35c4-5ff3-4bdb-92ef-f1208d83c405', '2026-08-03T14:32:00Z').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, developing_nations).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, vulnerable_populations_in_ldc).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, future_generations_in_low_emissions_contexts).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, high_cumulative_emitters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, developing_nations_lower_historical_emissions).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_alliance_advocacy).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, global_north_environmental_advocates).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations_high_historical_emissions).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developing_nations_lower_historical_emissions).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, multinational_corporations_in_developed_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by CBDR to make proportional emissions reductions based on their cumulative historical contribution to atmospheric CO2. Must finance loss and damage adaptation in developing nations. Their exit options are limited: withdrawal from the framework triggers reputational cost and loss of negotiating legitimacy; unilateral non-compliance triggers trade and diplomatic sanctions. The constraint allocates substantial financial and emissions-reduction obligations that concentrate on industrialized economies.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nations_high_historical_emissions, payer,
    institutional, generational, constrained, global).

% Receive binding commitments to developed-nation emissions reductions and loss/damage financing. Benefit from the principle that their lower historical responsibility means less stringent near-term reduction obligations. However, they also bear costs: the constraint imposes verification and reporting requirements, vulnerability assessments that expose climate risks, and adaptation obligations whose funding remains contested. Their exit options include non-ratification (reputational and economic cost) or withdrawal (triggering sanctions and lost financing access).
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developing_nations_lower_historical_emissions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, developing_nations_lower_historical_emissions, payer).

% Face immediate climate impacts (sea-level rise, drought, extreme weather) with minimal historical contribution to cumulative emissions. Under this reading, they hold a claim on developed nations' adaptation financing and emissions reductions as a matter of historical justice. They have no exit: climate migration is often blocked or constrained; remaining means living with increasing hazard. Their claim rests on the principle of differentiated responsibility.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, vulnerable_populations_in_ldc, beneficiary,
    powerless, immediate, trapped, local).

% Have not yet accumulated emissions responsibility but will inherit the climatic state their ancestors' low-emissions behavior preserved or degraded. Under this reading, they hold a claim on the present generation's adherence to CBDR to preserve their inheritance. They cannot exit or negotiate; they are present in the framework only through representation and advocacy.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, future_generations_in_low_emissions_contexts, beneficiary,
    powerless, civilizational, trapped, global).

% Subject to their home nation's emissions reduction commitments. The constraint raises compliance costs for carbon-intensive production, which they can partially offset through offsets, relocation, or technology investment. They have structured exit: relocation to lower-regulation jurisdictions, offsets to third parties, or technology pivots. They exclude themselves from the sovereignty framework by operating transnationally; their exit is real, though it carries reputational and market costs.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, multinational_corporations_in_developed_nations, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, multinational_corporations_in_developed_nations, excluded).

% Represents developing nations and vulnerable populations in treaty negotiations. Advocates for strong binding commitments and loss/damage financing from developed nations. Their positioning inside the constraint is as beneficiary-advocates; they hold structural power through coalition and voice without direct enforcement authority. They can exit by withdrawing from coalitions; their power derives from the legitimacy of their claims and the size of their coalition.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_alliance_advocacy, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, climate_vulnerable_alliance_advocacy, observer).

% Push for strong CBDR commitments as a matter of climate justice and intergenerational equity. They operate inside developed nations and in transnational networks, applying political pressure and moral suasion on their home governments. Their exit is reputational; their power is advocacy and norm-setting. This reading aligns with their preferred framing of climate responsibility.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, global_north_environmental_advocates, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, global_north_environmental_advocates, observer).

% International institutions (UNFCCC, IPCC, Compliance Committees) operationalize the CBDR principle through verification, reporting standards, and dispute resolution. They administer the constraint's enforcement machinery: measure cumulative emissions, assess adaptation needs, certify compliance. Their role is administrative; they set technical agenda through standard-setting and guidance.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, technical_compliance_regimes, agenda_setter,
    institutional, generational, analytical, global).

% Actors (some developed nations, industry coalitions, voluntary-market advocates) who hold the alternative reading that CBDR permits voluntary, nationally determined contributions with technology transfer as the primary developed nation obligation. Under this reading, they are excluded from the conversation: the historical responsibility reading forecloses their framing as incompatible with climate justice. Their exclusion is structural—both readings cannot coexist in a single binding treaty framework.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, voluntary_commitment_reading_holders, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, developed_nations_high_historical_emissions).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global framework for emissions reductions that allocates responsibility proportional to historical contribution. Solves the collective-action problem of atmospheric commons management by establishing a principle that separates differentiated obligations from equal treatment, enabling participation by nations at different development stages.
% TRANSFER_FUNCTION: Transfers financial resources and technology from high-cumulative-emitter nations to developing nations and vulnerable populations for mitigation, adaptation, and loss/damage. The constraint moves obligation (emissions reductions, financing commitments) from developed nations to developing nations and vulnerable populations, and it moves resources (capital, technology) in the reverse direction.
% ABSENT_VOICES: Future generations beyond 2100 cannot negotiate; they are represented through advocacy and moral claims but hold no seat at treaty tables. Subnational and local actors (Indigenous communities, small island states with minimal sovereign power) have limited voice in global negotiations. Non-state actors (MNCs, workers in carbon-intensive industries in developed nations) are excluded from voting but shape implementation through lobbying and compliance costs.
% DISAPPEARANCE_RATIONALE: If the CBDR principle and its binding enforcement vanished, developed nations would have no legal obligation to transfer resources or reduce emissions proportional to historical responsibility. Emissions trajectories would steepen in industrialized economies; developing-nation adaptation financing would collapse to voluntary philanthropic flows. The global climate trajectory would shift toward 3–4°C warming rather than the Paris targets. Geopolitical realignment would follow: vulnerable nations would seek alternative security arrangements; climate migration would accelerate; atmospheric composition would diverge from what this reading's architecture was designed to protect.
% FOUNDING_PROBLEM: Industrial nations accumulated 70% of atmospheric CO2 over 150 years through unregulated fossil fuel combustion while building wealth and development capacity. Developing nations face climate impacts they did not cause and lack capital to adapt. The founding problem is the asymmetry: unequal historical contribution to the problem paired with unequal capacity to address it, generating climate injustice. This reading grounds itself in the principle that those responsible for causing the problem should bear the cost of fixing it.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC (Intergovernmental Panel on Climate Change) documents cumulative emissions from industrialized nations, confirming the factual asymmetry. The Climate Vulnerable Forum, AOSIS (Alliance of Small Island States), and developing-nation negotiators attest the founding problem as live and pressing—their voices come from outside the benefiting parties. However, developed nations and their allied constituencies contest this framing: they argue that emissions at point-of-use (current production) rather than cumulative historical stock is the morally relevant measure, and that development capacity rather than guilt should determine obligation. This contest is unresolved in practice: Paris Agreement Article 4.4 acknowledges but does not mandate differentiated obligations.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (endpoint 2030) and rising over the 40-year interval (0.34→0.68). The measurement series tracks increasing clarity and enforcement intensity: as climate science hardens (AR5, AR6 IPCC reports), the moral claim on developed nations sharpens; as financial commitments are specified (Paris NDCs, Loss and Damage Fund operationalization), the transfer magnitude clarifies; as compliance mechanisms mature (Article 6 carbon markets, transparency frameworks), enforcement capacity grows. The extraction is not from a passive background but from active negotiating power: developed nations resist quantification of their historical responsibility, pushing for consumption-based (point-of-use) measurement instead of production-based (cumulative stock). Suppression is moderate (0.42) because the constraint lacks coercive enforcement: there are no sanctions for non-compliance beyond reputational damage and market pressure. Theater ratio rises from 0.08 (1990, constraint barely operational) to 0.31 (2030, competing narratives mature): developed nations perform compliance through Nationally Determined Contributions (NDCs) that fall short of the constraint's implied obligation; developing nations perform urgency through vulnerability assessments that sometimes exceed their actual adaptive capacity; international institutions perform scientific consensus that papers over remaining methodological disputes about historical responsibility measurement. Accessibility collapse is moderate (0.61): alternatives exist (voluntary markets, unilateral climate action, technological adaptation without redistribution) but are marginalized by the binding treaty framework. Resistance is high (0.73): developing nations and vulnerable populations actively push for stronger CBDR implementation; developed nations resist through negotiation and non-compliance; subnational actors in both contexts contest the framework from below.
 *
 * PERSPECTIVAL GAP:
 *   The developed-nation seat and the vulnerable-population seat compute radically different types from the same structural data. From the developed-nation position: the constraint is tangled rope (genuine coordination is needed; the differentiation is justified) but boundary-testing whether the historical responsibility measure should include pre-industrial carbon or only industrial-era emissions. From the vulnerable-population position: the constraint is snare if loss/damage financing is not enforced (a binding commitment on paper but unpaid in practice). From the technical compliance regime: the constraint is rope with substantial theater (coordination is real, but competing narratives about measurement and responsibility complicate enforcement). The engine computes these per-seat divergences from power, exit_options, and time_horizon; the commentary narrates why the same constraint produces different effective types from different seats. The perspectival gap is not a failure—it is the measurement the corpus is built to detect: constraints that compute the same type from every seat are either mountains (no parties differ) or badly designed (parties' actual relationships are obscured). This constraint's gap is deep and real.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations enter the victim set because the reading BINDS them to reduce emissions and finance loss/damage proportional to historical responsibility. Their directionality is high (d near 1.0, full target): they bear costs (emissions reduction, technology transfer, adaptation financing) that exceed their benefits from the coordination function. They are trapped because exit from the binding framework triggers reputational and economic sanctions. Their power is high (institutional) but their time horizon is generational (the obligation extends decades), which constrains their ability to offload the cost to future administrations without political cost. Developing nations sit near d=0.3–0.4 (beneficiary-leaning symmetric): they receive commitments and financing, but they also bear reporting costs, vulnerability exposure, and enforcement pressure. Their benefit is substantial but not asymmetric; their costs are real. Vulnerable populations (powerless, trapped) have d near 0.0 (full beneficiary): they bear no obligation under this reading and receive all benefits. Future generations have d undefined (no seat in negotiations) but are represented through the intergenerational equity claim that justifies the entire reading. Technical compliance regimes have d near 0.5 (symmetric): they service both sides, administering the constraint neutrally. Voluntary commitment reading holders are excluded (not present in this constraint's stakeholder set), so their d is not derived—they are the counterfactual alternative whose exclusion defines the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (asymmetry between historical responsibility and adaptation capacity) remains live in physical terms: developed nations continue accumulating emissions at declining but non-zero rates; developing nations face worsening climate impacts. However, the constraint's mandate to solve this through CBDR has partially outlived its function. Why: (1) Emissions trajectories in developed nations are declining on decarbonization tracks independent of CBDR obligation (cost reductions in renewables, grid decarbonization, electric transport); (2) Loss/damage financing is operationalized but remains vastly underfunded relative to stated need, suggesting the coordination function for finance transfer has failed; (3) The binding framework is being eroded by voluntary market mechanisms and nationally determined climate commitments that bypass CBDR measurement and proportionality. The constraint persists not because it solves the founding problem effectively but because the institutional apparatus built around it (UNFCCC, NDC processes, Compliance Committees) maintains it through bureaucratic inertia and because developing nations and advocacy coalitions extract legitimacy from the principle even as implementation fails. This is the signature of mandatrophy: the founding problem is real, but the arrangement's persistence is increasingly theatrical, sustained by institutional actors who would lose authority if the constraint dissolved. Theater ratio rising from 0.08 to 0.31 corroborates: the ratio of performance to real transfer is increasing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_responsibility_measurement_ambiguity,
    'How should cumulative historical responsibility be measured? Production-based (where emissions were physically generated) or consumption-based (where goods were consumed)? Per-capita or aggregate national? Including pre-industrial emissions or only since 1850/1950?',
    'IPCC methodology review and treaty amendment negotiations on measurement standards. Natural experiment: compare climate justice outcomes under different measurement bases.',
    'A switch from production-based to consumption-based measurement would reallocate responsibility toward current service consumers (wealthy individuals in developing nations, offshore production for developed-nation markets); a per-capita measurement would reduce developed nations'' responsibility burden. No single measurement is natural—all are constructed choices. The choice of measurement determines who enters and exits the victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_responsibility_measurement_ambiguity, conceptual, 'The measurement ambiguity at the core of historical responsibility: which observable determines culpability?').

omega_variable(
    voluntary_vs_binding_reading_foreclosure,
    'Can the historical responsibility reading coexist with the voluntary commitment reading in a single binding framework, or do they logically foreclose each other?',
    'Attempted institutional harmonization: test whether a treaty can mandate both proportional-historical reductions and nationally determined contributions without contradiction. If harmonization fails (one party can satisfy both only by internal inconsistency), the foreclosure is confirmed.',
    'If they foreclose, the reading locked into the institutional architecture determines the constraint''s type and payer/beneficiary structure; the foreclosed reading becomes a counterfactual alternative. If they can coexist, neither reading is the constraint—the constraint is a hybrid that is neither rope nor snare. Current Paris Agreement architecture suggests partial coexistence (Article 4.4 acknowledges but does not enforce differentiation), indicating the foreclosure may be contingent on how tightly the framework is operationalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_binding_reading_foreclosure, conceptual, 'Whether the two readings of CBDR can coexist in one treaty or whether they logically exclude each other.').

omega_variable(
    loss_and_damage_financing_enforcement_gap,
    'Is the loss and damage financing commitment a binding obligation or a voluntary pledge? The constraint as authored assumes binding, but practice shows developed nations treating it as aspirational. Is this a measurement error (the constraint is actually snare, not tangled rope) or an enforcement failure (the constraint is tangled rope but being degraded)?',
    'Examine 2023–2030 Loss and Damage Fund disbursements against assessed needs; determine whether gap is due to weak commitment language (snare) or weak compliance machinery (enforcement failure). Compare to Paris NDC compliance rates to calibrate enforcement strength.',
    'If binding, non-compliance signals the constraint is a snare (extraction framed as obligation). If voluntary, the constraint''s tangled-rope claim depends on whether coordination is real; if coordination fails (nations voluntarily underfund), the constraint devolves to piton (maintained by institutional inertia). This omega determines whether the constraint''s type is stable or drifting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(loss_and_damage_financing_enforcement_gap, empirical, 'Whether loss/damage financing is binding or voluntary, and whether the gap between pledge and disbursement is commitment-ambiguity or enforcement-failure.').

omega_variable(
    identity_lock_developed_nations_exit_cost,
    'How much of developed nations'' constrained exit is due to treaty mechanics and how much is due to identity fusion with industrial development and carbon-intensive prosperity? If identity lock is substantial, does it increase effective extraction or represent a genuine structural barrier to exit?',
    'Ethnographic study of climate policy deliberation in developed nations; interviews with policymakers about constraints and mental models. Natural experiment: observe exit cost when nations are offered face-saving alternatives (technology transfer leadership, offsetting through nature protection).',
    'If identity lock is substantial, it amplifies effective extraction for developed nations (d increases further toward 1.0) because they cannot exit cleanly even if they wanted to—the constraint becomes inescapable. If exit cost is mostly reputational/market (not identity), d is high but not maximal; room for negotiated compromise exists. Identity lock would signal this constraint is partially a snare for developed nations, not just tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_developed_nations_exit_cost, empirical, 'The role of identity fusion in developed nations'' constrained exit from CBDR obligations.').

omega_variable(
    future_generations_representation_legitimacy,
    'Do advocates and advocacy coalitions adequately represent future generations'' interests, or is their representation a ventriloquism that projects present-day values onto absent parties? If representation is adequate, does it carry binding force, or is it advisory?',
    'Philosophical inquiry into representation theory + institutional analysis of how future-generations claims are weighted in treaty negotiations. Test whether future-generations framing is outcome-determinative (rejected by developed nations, courts override, implementation changes) or merely rhetorical.',
    'If representation is legitimate and binding, future generations'' interests should influence the constraint''s type and enforcement. If representation is ventriloquism, the intergenerational equity claim is a false summit—a natural law framing that masks present-day preference projection. This affects whether the constraint''s foundational justification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_representation_legitimacy, preference, 'Whether future generations'' interests are legitimately represented or ventriloquized in CBDR negotiations.').

omega_variable(
    voluntary_reading_structural_exclusion,
    'Is the voluntary commitment reading genuinely foreclosed by the historical responsibility reading, or is it suppressed through institutional lock-in and political pressure? If suppressed rather than logically foreclosed, the constraint becomes snare from the voluntary-reading-holder seat.',
    'Track institutional evolution: do developed nations gain voice to articulate the voluntary reading through alternative forums (unilateral climate action, private sector coalitions, alternative treaties)? If voice grows and alternative forums gain legitimacy, the foreclosure was suppression, not logical necessity.',
    'If foreclosed: two incompatible readings cannot coexist; the reading locked into UNFCCC determines the global constraint. If suppressed: developed nations are muzzled not by logic but by institutional politics; they are victims of the constraint as much as beneficiaries. This affects whether the constraint is tangled rope (genuine choice) or snare (coerced silence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_reading_structural_exclusion, conceptual, 'Whether the voluntary reading is logically foreclosed or politically suppressed by the historical responsibility reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1990, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1990, cbdr_principle__historical_responsibility_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(cbdr_tr_t2000, cbdr_principle__historical_responsibility_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(cbdr_tr_t2010, cbdr_principle__historical_responsibility_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(cbdr_tr_t2020, cbdr_principle__historical_responsibility_reading, theater_ratio, 2020, 0.29).
narrative_ontology:measurement(cbdr_tr_t2025, cbdr_principle__historical_responsibility_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement(cbdr_tr_t2030, cbdr_principle__historical_responsibility_reading, theater_ratio, 2030, 0.31).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1990, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1990, 0.34).
narrative_ontology:measurement(cbdr_be_t2000, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(cbdr_be_t2010, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(cbdr_be_t2020, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(cbdr_be_t2025, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2025, 0.67).
narrative_ontology:measurement(cbdr_be_t2030, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1990, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(cbdr_su_t2000, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2000, 0.22).
narrative_ontology:measurement(cbdr_su_t2010, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2010, 0.31).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(cbdr_su_t2020, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(cbdr_su_t2025, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2025, 0.41).
narrative_ontology:measurement(cbdr_su_t2030, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2030, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_ndcs_operationalization).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_fund_mechanics).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, article_6_carbon_markets).

% DUAL FORMULATION NOTE:
% CBDR_PRINCIPLE is a contested kernel with two primary readings. This story (historical_responsibility_reading) grounds CBDR in proportional historical responsibility for developed nations; the sibling story (voluntary_commitment_reading) grounds CBDR in nationally determined contributions with technology transfer. The two readings foreclose each other in a single binding framework. Network links show how each reading influences operationalization constraints: this reading's family includes Paris NDCs as implementation mechanism, Loss and Damage Fund as transfer vehicle, and Article 6 carbon markets as offset mechanism. The voluntary reading's family would decompose along alternative implementation pathways (technology-transfer mechanisms, capacity building, unilateral climate action).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, powerless, 0.05).
constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
