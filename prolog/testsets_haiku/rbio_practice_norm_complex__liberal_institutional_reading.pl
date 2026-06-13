% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Norms as Universal Consent-Based Framework (Liberal Institutional Reading)
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   The RBIO (Responsibility to Protect, International Humanitarian Order)
 *   norm complex is presented in liberal institutional discourse as a
 *   universal, consent-based framework for legitimate intervention: military
 *   action and economic coercion are justified when authorized by the UNSC or
 *   when grave atrocities demand humanitarian response. This reading treats
 *   enforcement selectivity (the fact that interventions are authorized
 *   inconsistently, some atrocities go unaddressed, some powerful states face
 *   no consequences) as a capacity problem—limited institutional resources,
 *   political constraints, and procedural limitations prevent enforcement
 *   from being truly universal, but the underlying norms remain legitimate
 *   because they are consensual and revisable through multilateral processes.
 *   This constraint story instantiates that reading and its structural
 *   consequences: beneficiaries (intervening states, contractors,
 *   humanitarian advocacy networks), victims (targeted states, sanctioned
 *   civilians), and the asymmetric extraction of sovereignty through coerced
 *   consent. The claim/metric divergence is strategic: this reading CLAIMS
 *   the arrangement as a rope (genuine coordination solving a real
 *   collective-action problem about atrocity prevention) while the authored
 *   metrics describe a substantially extractive, actively enforced
 *   tangled_rope (extraction riding on coordination). The engine measures
 *   this divergence; the reading does not adjudicate it. The sibling readings
 *   (hegemonic_extraction, sovereignty_maximalist) instantiate different
 *   committer positions and would author substantially different metrics on
 *   the same kernel.
 *
 * KEY AGENTS:
 *   - intervening_state_coalitions: institutional power, arbitrage exit, set the enforcement agenda and interpretation of UNSC authorization; agenda-setters who benefit from legitimacy for their own interventions
 *   - targeted_state_populations: powerless, trapped exit, structurally unable to consent or exit; victims of intervention and sanctions; narrative exclusion from norm-production forums
 *   - multilateral_institutions (UNSC, General Assembly, ICJ): institutional power, analytical exit, administer the norm framework and determine when enforcement selectivity is justified; structural beneficiaries from norm legitimacy and institutional continuity
 *   - sovereignty_minimizing_states (Russia, China, non-aligned powers): institutional power, constrained exit, excluded from P5 veto reshaping and enforcement-selectivity decisions; would contest the liberal reading if given voice
 *   - reconstruction_contractors: powerful, mobile exit, capture contracts flowing from intervention legitimacy; private-sector beneficiaries of the norm framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.58).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.62).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Norms as Universal Consent-Based Framework (Liberal Institutional Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '4a520fec-9d41-44a8-919c-82b0054c24ae').
narrative_ontology:cs_kernel_codification('4a520fec-9d41-44a8-919c-82b0054c24ae', fixed_text).
narrative_ontology:cs_authority_grounding('4a520fec-9d41-44a8-919c-82b0054c24ae', extraction).
narrative_ontology:cs_interpretation_layer_present('4a520fec-9d41-44a8-919c-82b0054c24ae').
narrative_ontology:cs_reading_relation('4a520fec-9d41-44a8-919c-82b0054c24ae', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a520fec-9d41-44a8-919c-82b0054c24ae', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('4a520fec-9d41-44a8-919c-82b0054c24ae', foundational, humanitarian_intervention_legitimacy_universal).
narrative_ontology:cs_axiom_status(humanitarian_intervention_legitimacy_universal, holdable).
narrative_ontology:cs_axiom_grounding('4a520fec-9d41-44a8-919c-82b0054c24ae', humanitarian_intervention_legitimacy_universal, deontological).
narrative_ontology:cs_axiom('4a520fec-9d41-44a8-919c-82b0054c24ae', foundational, enforcement_selectivity_capacity_not_intent).
narrative_ontology:cs_axiom_status(enforcement_selectivity_capacity_not_intent, holdable).
narrative_ontology:cs_axiom_grounding('4a520fec-9d41-44a8-919c-82b0054c24ae', enforcement_selectivity_capacity_not_intent, empirically_contingent).
narrative_ontology:cs_axiom('4a520fec-9d41-44a8-919c-82b0054c24ae', secondary, multilateral_consent_binding_despite_duress).
narrative_ontology:cs_axiom_status(multilateral_consent_binding_despite_duress, holdable).
narrative_ontology:cs_axiom_grounding('4a520fec-9d41-44a8-919c-82b0054c24ae', multilateral_consent_binding_despite_duress, conventional).
narrative_ontology:cs_reference_frame('4a520fec-9d41-44a8-919c-82b0054c24ae', universal_humanitarian_intervention_framework).
narrative_ontology:cs_drift_state('4a520fec-9d41-44a8-919c-82b0054c24ae', contemporary_selective_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4a520fec-9d41-44a8-919c-82b0054c24ae', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_state_coalitions).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_advocacy_networks).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, economically_sanctioned_civilians).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_minimizing_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, reconstruction_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, global_north_publics).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce RBIO norms through military intervention (with UNSC authorization or under humanitarian exception doctrine) and economic sanctions. Justify intervention as protecting universal values and civilians from atrocities. Benefit from norm-framework legitimacy for their own security interests and from contracts flowing to their firms during reconstruction. Set the interpretive bar for what counts as UNSC authorization or grave atrocity threshold.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_state_coalitions, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to military intervention, economic sanctions, and humanitarian conditionality. Structurally unable to consent to or exit the regime without bearing massive costs. Told the intervention protects them; experience it as occupation, sovereignty loss, and civilian casualties. No seat at the negotiating table that produces the norms they are subject to.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_populations, payer,
    powerless, biographical, trapped, global).

% Formally invited to consent to RBIO norms through multilateral processes (General Assembly, regional forums); practically unable to refuse without triggering sanctions or intervention. Their consent is elicited under duress (the threat of enforcement). Excluded from the inner councils (P5 UNSC seat) that decide enforcement selectivity and determine what counts as legitimate grounds for intervention.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments, excluded).

% Administer the RBIO norm framework (UNSC, General Assembly, ICJ, regional human rights bodies). Derive legitimacy from being the sites where consent is supposedly channeled and norms are supposedly revisable. Set the procedures for amendment and the criteria for enforcement selectivity interpretation. Benefit from institutional continuity and the authority structure that treats them as the legitimate loci of norm-production.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutions, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutions, agenda_setter).

% Advocate for civilian protection and human rights enforcement within the RBIO framework. Gain legitimacy and resources by framing interventions as necessary humanitarian responses. Can exit by shifting to different norm frameworks or by ceasing advocacy if they withdraw consensus, but derive their mission definition and institutional position from the existence of these norms.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Bear the direct costs of economic sanctions imposed under RBIO legitimacy (medicine shortages, food insecurity, currency collapse). Are told sanctions are contracts to which their government consented; experience them as collective punishment. Have no representation in the sanctioning decision and cannot exit by individual defection without state-level legal repercussions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, economically_sanctioned_civilians, payer,
    powerless, immediate, trapped, global).

% States formally outside the liberal multilateral consensus (Russia, China, other non-aligned powers). Argue that RBIO norms privilege Western institutional preferences and constrain state autonomy. Structurally excluded from reshaping the framework (unable to amend P5 veto or UNSC procedures without explicit consent from the powers that benefit from current architecture). Would argue for sovereignty maximalism if present in the consent-production process.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_minimizing_states, excluded,
    powerful, generational, constrained, global).

% Capture contracts for post-intervention reconstruction, security privatization, and infrastructure rebuilding. Benefit directly from the intervention pipeline; secured through RBIO legitimacy. Can operate in multiple intervention contexts and exit particular ones without losing access to the overall extraction mechanism.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, reconstruction_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from security narratives framed around universal RBIO norms (framing interventions as collective protection rather than resource extraction). Enjoy goods and services produced by contractors operating under intervention legitimacy. Can mobilize to withdraw consensus through domestic political pressure, though institutional inertia makes this costly.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, global_north_publics, beneficiary,
    organized, biographical, mobile, global).

% Hold veto power over UNSC authorization of interventions. Set the enforcement agenda by selective use of veto. In this reading, treated as trustees of the universal framework, exercising veto as a guard against illegitimate intervention; in competing readings, their veto is treated as hegemonic control. Their power to define enforcement selectivity is technically procedural (capacity) but structurally permanent.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, unsc_permanent_members, agenda_setter,
    institutional, civilizational, analytical, global).

% Document and interpret RBIO norms, producing the legitimacy narratives that justify interventions and sanctions. In this reading, treat selectivity as an implementation problem solvable by better procedures; in competing readings, interpret selectivity as evidence of hidden extraction. Can shift framings but have limited power to reshape the underlying enforcement architecture.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal framework for when cross-border military intervention and economic coercion are legitimate: to prevent atrocities, uphold human rights, and protect civilians from genocidal violence. Solves the coordination problem of preventing free-riding on humanitarian intervention (some states benefit from protection while others bear the cost of provision) and of distinguishing legitimate collective action from disguised hegemonic domination.
% TRANSFER_FUNCTION: Moves sovereignty, resources, and legitimacy from targeted states to intervening coalitions and their contractors. Targeted states accept economic conditionality (sanctions, debt-for-reform programs) and military presence (occupation, bases, training missions) as the price of access to multilateral legitimacy and reconstruction finance. Contractors capture reconstruction rents. Intervening states gain security legitimacy and geopolitical positioning.
% ABSENT_VOICES: Sovereignty-maximalist states (Russia, China, non-aligned powers) are structurally excluded from reshaping the P5 veto and UNSC procedures that define enforcement selectivity. They would argue that RBIO norms are used selectively to constrain rising powers while protecting the current order, and that enforcement selectivity reveals hegemonic intent rather than capacity constraints. Targeted state populations have no collective seat at norm-production tables (General Assembly votes are formal consent but decisions are made in P5 closed sessions). Alternative frameworks emphasizing sovereignty over humanitarian intervention are suppressed by the institutional power of the liberal multilateral machinery.
% DISAPPEARANCE_RATIONALE: If RBIO norms and their enforcement machinery disappeared, military intervention would lose the legitimacy cover it currently enjoys; states would need to openly defend interventions on realpolitik grounds or abandon them. Reconstruction contracts would shift from post-intervention reconstruction to other investment channels. Targeted states would recover formal sovereignty (though not material capacity). Humanitarian protection would depend on ad-hoc coalitions rather than an institutionalized norm framework. The liberal multilateral order would lose its core legitimacy mechanism.
% FOUNDING_PROBLEM: In the post-WWII order, states needed a shared framework to distinguish legitimate collective security (protecting against aggression, preventing atrocities) from illegitimate coercive intervention (regime change, resource extraction). The RBIO framework was built to enshrine consent, universal applicability, and legitimate authority (UNSC) as guards against hegemonic abuse.
% FOUNDING_PROBLEM_CORROBORATION: The liberal institutional reading attests the founding problem is live: atrocities still occur, states still need collective action frameworks to respond legitimately. Sovereignty-maximalist states and non-aligned powers attest the problem is solved differently (or not solved at all): RBIO norms are used selectively by the same hegemonic powers the framework was meant to constrain. Targeted state governments attest RBIO norms are mechanisms of coerced consent, not legitimate protection. Human rights networks attest the founding problem persists but the solution is often inadequate. Scholarly consensus is split: liberal internationalists emphasize implementation problems (enforcement selectivity as capacity constraint); critical scholars emphasize structural extraction (enforcement selectivity as hegemonic intent). No uncontested external corroboration exists; the founding problem itself is constituted by the contest between readings.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.58 at interval end) reflects the flow of sovereignty, resources, and legitimacy from targeted states to intervening coalitions, measured as substantial but not total domination—targeted states retain nominal statehood and can theoretically refuse consent, but refusal triggers enforcement (intervention, sanctions, isolation). Suppression (0.62) is slightly higher because the constraint's persistence depends on actively excluding alternative frameworks (sovereignty maximalism) and suppressing the narrative that enforcement selectivity reveals hegemonic intent. Theater ratio (0.41, moderate-low) reflects the real coordination function (preventing atrocities, establishing rules for intervention) alongside growing performative maintenance: humanitarian rhetoric expanding to cover geopolitical interventions, security privatization presenting as reconstruction aid, and the consent production process becoming increasingly ritualized (formal General Assembly votes with binding outcomes determined in P5 closed sessions). The measurement series shows steady accumulation from 1945 (post-WWII institutional founding) through 2024: extractiveness rose from 0.35 to 0.58 as the RBIO framework expanded from Cold War stalemate through interventionism (1990s+) to contemporary sanctioning regimes; theater grew as humanitarian framing became more essential to intervention legitimacy post-Iraq; suppression increased as non-aligned resistance to the framework grew and required more active institutional delegation to manage. The coercion grid captures level-resolved pressure: structural-level (system-wide) pressure on state sovereignty rose steeply (0.35→0.48 accessibility collapse, reflecting the narrowing of acceptable state behavior); organizational-level pressure (state-government accountability) rose faster (0.42→0.56); class-level pressure on targeted populations (collective punishment via sanctions) rose (0.38→0.51); individual-level pressure was lowest throughout (0.32→0.44), reflecting the diffusion of sanction costs and the distance of individual civilians from norm-production decisions. Resistance followed: lowest at individual level (fragmented, unorganized), highest at organizational level (state governments and non-aligned blocs actively contesting framework expansion).
 *
 * PERSPECTIVAL GAP:
 *   The intervening-state-coalition seat and the targeted-state-population seat should compute dramatically different types from identical structural facts. From the coalition seat, the arrangement looks like rope (genuine coordination solving an atrocity-prevention collective-action problem); constraints on enforcement are capacity problems solvable by institutional reform, not legitimacy problems. From the targeted-population seat, the same facts look like snare (coerced subordination covered by humanitarian rhetoric; no real exit; sovereignty lost without consent). From the multilateral-institution seat, it looks like organized-rope (they administer the coordination function and benefit from institutional continuity). From the sovereignty-maximalist-state seat (excluded from the reading but present in the kernel contest), the framework looks like hegemonic tangled_rope at best (extraction using humanitarian cover), snare at worst (coerced regime change). The engine computes per-seat classification from power+exit+directionality; the authored metrics are story-level averages and will not reflect this divergence directly. The divergence emerges in the seat-by-seat computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening state coalitions are structural beneficiaries (d near 0.0): they set the enforcement agenda, control P5 veto, extract legitimacy for their security interests, and capture reconstruction contracts. Their exit is arbitrage-grade (can choose to intervene or not; can switch between intervention targets; bear no cost if they withdraw). Targeted states are structural targets (d near 1.0): they bear intervention costs (military casualties, sovereignty loss, occupation), sanctions costs (economic hardship, civilian suffering), and conditionality costs (policy dictation). Their exit is trapped (cannot leave the international system; cannot refuse RBIO norms without triggering enforcement). Multilateral institutions sit at moderate extraction beneficiary (d ~0.2): they benefit from institutional continuity and legitimacy as norm-adjudicators, but they are also constrained by P5 veto and have limited power to reshape enforcement procedures without intervening-state consent. Humanitarian advocacy networks sit near beneficiary (d ~0.1): they benefit from norm existence and institutional position, can exit by shifting to different advocacy, and are partly captured by intervening-state framing of humanitarian need. Reconstruction contractors are beneficiaries (d ~0.0): they extract rents from intervention legitimacy and can exit by switching to non-intervention contexts. Sanctioned civilians are targets (d near 1.0, but diffusely so): they bear costs but have no representation in sanctioning decisions. No directionality overrides are needed here; the derivation from beneficiary/victim + exit produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the false-summit trap (claiming rope when measuring tangled_rope or snare) by explicitly claiming tangled_rope—the reading acknowledges both coordination (genuine prevention of atrocities, collective security) and extraction (asymmetric flow of sovereignty to interveners, coerced consent from targets). The coordination function is not a cover story; it solves a real collective-action problem. The extraction is not negligible; it is substantial and structural. The tension between them is managed by the reading's core claim: enforcement selectivity is a capacity problem, not a legitimacy problem—the coordination function is sound, implementation is imperfect. This framing protects the reading from mandatrophy (the constraint's founding problem outliving its function) by proposing that the founding problem (preventing atrocities, establishing legitimate intervention rules) is still live and the constraint persists because it solves it. But the measurement series (rising extractiveness over time, theater ratio moving upward) suggests potential mandatrophy: the extraction is growing faster than the coordination function is delivering new value. A rival reading would argue the founding problem is now dead (interventions are routine, atrocities are ignored when inconvenient, humanitarian rhetoric is theatre) and the constraint persists as pure inertia—this is the hegemonic_extraction reading's thesis. The liberal reading's defense against mandatrophy hinges on whether enforcement selectivity is genuinely a capacity problem. If it can be empirically shown to follow from resource constraints and procedural limitations (resolution mechanism in omega_enforcement_selectivity_interpretation), mandatrophy is avoided. If selective enforcement reflects structural hegemonic intent (the counter-thesis), mandatrophy becomes likely and the reading collapses into a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_interpretation,
    'Is enforcement selectivity a capacity problem (genuine constraints on institutional resources and political will to enforce universally) or a legitimacy problem (structural hegemonic intent to enforce selectively to maintain power distribution)?',
    'Comparative institutional analysis: examine whether enforcement selectivity follows from capacity-constrained resource allocation (testable via resource distribution data and decision-procedure transparency) or from structured power asymmetry (testable via veto patterns, cost-benefit analysis of enforcement targets, and alternative institutional designs that would be capacity-neutral but hegemony-neutral).',
    'If capacity: RBIO norms remain legitimate in principle; enforcement selectivity is a solvable implementation problem. If legitimacy: RBIO norms are a hegemonic institution whose universality is theater; the sibling hegemonic_extraction_reading becomes more plausible structurally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_selectivity_interpretation, empirical, 'The core interpretive crux between liberal institutional and hegemonic extraction readings.').

omega_variable(
    consent_under_duress_paradox,
    'When targeted states formally consent to RBIO norms and sanctions regimes under the threat of military intervention or intensified sanctions (explicit or implicit), is that consent valid for legitimacy purposes, or does duress-induced consent vitiate the legitimacy claim?',
    'Philosophical/legal analysis of the legitimacy standards applied to consent in other domains (contract law, sexual consent, political subordination); empirical analysis of state statements on motivation for consent and counterfactual state preference if exit were cost-free.',
    'If valid despite duress: consent-based legitimacy holds and targeted states cannot claim victimhood at the norm level (only at implementation level). If invalid: the norm framework rests on coerced agreement and the sovereignty_maximalist_reading''s critique of RBIO as a cover for regime change gains structural weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_duress_paradox, conceptual, 'The structural legitimacy of consensual agreement under implicit coercion.').

omega_variable(
    universal_norms_vs_western_institutional_path,
    'Are RBIO norms truly universal and context-neutral, or do they encode the institutional preferences and cultural assumptions of Western liberalism, making them incompatible with alternative governance models that non-aligned powers advocate?',
    'Comparative legal and institutional analysis: identify elements of RBIO norm framing that are derived from Western democratic and human-rights philosophy versus elements that would be endorsed by non-aligned institutional frameworks; test whether non-aligned powers refuse consent because of structural extraction or because of genuine value incommensurability.',
    'If universal: non-aligned resistance is strategic (refusing legitimate norms to preserve hegemony); this reading holds. If culturally embedded: RBIO norms are a universal imposition of Western values; the sovereignty_maximalist reading becomes more defensible as a genuine alternative framework rather than a cover for non-compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_norms_vs_western_institutional_path, conceptual, 'Whether RBIO norms are culturally contingent or truly universal.').

omega_variable(
    reconstruction_contractor_capture,
    'How much of the extractiveness measured in this constraint flows to reconstruction contractors and security privatization (a side effect of intervention legitimacy) versus to intervening state governments directly (security positioning, geopolitical gain)?',
    'Empirical analysis of reconstruction contract allocation post-intervention: measure the share of contracts flowing to firms domiciled in intervening states, compare to counterfactual market allocation, and measure whether intervention decision-making incorporates contractor interests in the selection of targets or intensity of intervention.',
    'If contractor capture is dominant: the beneficiary is functionally a private layer separate from state interest, and the extraction is more privatized-snare-like; if state-centered: the extraction is more clearly a transfer of sovereignty to intervening state coalitions. The measured extractiveness remains the same; the pathology of the transfer changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_contractor_capture, empirical, 'The institutional structure of who captures the gains from intervention legitimacy.').

omega_variable(
    sibling_reading_committer_distinction,
    'This constraint instantiates ONE reading of the contested RBIO kernel. Two sibling readings (hegemonic_extraction_reading and sovereignty_maximalist_reading) instantiate different normative commitments about what the RBIO norms are and do. Do these readings genuinely coexist as live positions in international discourse, or does one reading''s dominance structurally foreclose the others?',
    'Institutional ethnography of norm-production forums (UNSC, General Assembly, ICJ, regional institutions, academic conferences): document which reading each party advances, whether reading-switching occurs as political conditions change, and whether any reading is formally or informally banned from legitimacy discourse.',
    'If genuinely coexist: all three readings are live and the committer structure holds; readers should attend to which reading is operative in any given context. If one forecloses the others: the committer framing is inaccurate and the constraint should be collapsed into a single dominant reading. Coexistence status affects how readers should interpret the omega about enforcement selectivity interpretation (is it genuinely underdetermined, or is one reading''s interpretation correct and the others are tactical misreadings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_committer_distinction, empirical, 'Whether the three sibling readings genuinely coexist or one dominates and marginalizes the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement_basis(rbio_tr_t1945, projected).
narrative_ontology:measurement(rbio_tr_t1975, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement_basis(rbio_tr_t1975, observed).
narrative_ontology:measurement(rbio_tr_t1991, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1991, 0.33).
narrative_ontology:measurement_basis(rbio_tr_t1991, observed).
narrative_ontology:measurement(rbio_tr_t2003, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement_basis(rbio_tr_t2003, observed).
narrative_ontology:measurement(rbio_tr_t2011, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2011, 0.4).
narrative_ontology:measurement_basis(rbio_tr_t2011, observed).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(rbio_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(rbio_be_t1945, projected).
narrative_ontology:measurement(rbio_be_t1975, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1975, 0.42).
narrative_ontology:measurement_basis(rbio_be_t1975, observed).
narrative_ontology:measurement(rbio_be_t1991, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1991, 0.48).
narrative_ontology:measurement_basis(rbio_be_t1991, observed).
narrative_ontology:measurement(rbio_be_t2003, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2003, 0.54).
narrative_ontology:measurement_basis(rbio_be_t2003, observed).
narrative_ontology:measurement(rbio_be_t2011, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2011, 0.56).
narrative_ontology:measurement_basis(rbio_be_t2011, observed).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(rbio_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.42).
narrative_ontology:measurement_basis(rbio_su_t1945, projected).
narrative_ontology:measurement(rbio_su_t1975, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1975, 0.48).
narrative_ontology:measurement_basis(rbio_su_t1975, observed).
narrative_ontology:measurement(rbio_su_t1991, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1991, 0.54).
narrative_ontology:measurement_basis(rbio_su_t1991, observed).
narrative_ontology:measurement(rbio_su_t2003, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2003, 0.59).
narrative_ontology:measurement_basis(rbio_su_t2003, observed).
narrative_ontology:measurement(rbio_su_t2011, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2011, 0.61).
narrative_ontology:measurement_basis(rbio_su_t2011, observed).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(rbio_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2024
narrative_ontology:measurement(rbio_grid_01, rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse(class), 1945, 0.38).
narrative_ontology:measurement(rbio_grid_02, rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse(class), 2024, 0.51).
narrative_ontology:measurement(rbio_grid_03, rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse(individual), 1945, 0.32).
narrative_ontology:measurement(rbio_grid_04, rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse(individual), 2024, 0.44).
narrative_ontology:measurement(rbio_grid_05, rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse(organizational), 1945, 0.42).
narrative_ontology:measurement(rbio_grid_06, rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse(organizational), 2024, 0.56).
narrative_ontology:measurement(rbio_grid_07, rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse(structural), 1945, 0.35).
narrative_ontology:measurement(rbio_grid_08, rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse(structural), 2024, 0.48).
narrative_ontology:measurement(rbio_grid_09, rbio_practice_norm_complex__liberal_institutional_reading, resistance(class), 1945, 0.55).
narrative_ontology:measurement(rbio_grid_10, rbio_practice_norm_complex__liberal_institutional_reading, resistance(class), 2024, 0.72).
narrative_ontology:measurement(rbio_grid_11, rbio_practice_norm_complex__liberal_institutional_reading, resistance(individual), 1945, 0.42).
narrative_ontology:measurement(rbio_grid_12, rbio_practice_norm_complex__liberal_institutional_reading, resistance(individual), 2024, 0.65).
narrative_ontology:measurement(rbio_grid_13, rbio_practice_norm_complex__liberal_institutional_reading, resistance(organizational), 1945, 0.52).
narrative_ontology:measurement(rbio_grid_14, rbio_practice_norm_complex__liberal_institutional_reading, resistance(organizational), 2024, 0.75).
narrative_ontology:measurement(rbio_grid_15, rbio_practice_norm_complex__liberal_institutional_reading, resistance(structural), 1945, 0.48).
narrative_ontology:measurement(rbio_grid_16, rbio_practice_norm_complex__liberal_institutional_reading, resistance(structural), 2024, 0.62).
narrative_ontology:measurement(rbio_grid_17, rbio_practice_norm_complex__liberal_institutional_reading, stakes_inflation(class), 1945, 0.32).
narrative_ontology:measurement(rbio_grid_18, rbio_practice_norm_complex__liberal_institutional_reading, stakes_inflation(class), 2024, 0.55).
narrative_ontology:measurement(rbio_grid_19, rbio_practice_norm_complex__liberal_institutional_reading, stakes_inflation(individual), 1945, 0.25).
narrative_ontology:measurement(rbio_grid_20, rbio_practice_norm_complex__liberal_institutional_reading, stakes_inflation(individual), 2024, 0.48).
narrative_ontology:measurement(rbio_grid_21, rbio_practice_norm_complex__liberal_institutional_reading, stakes_inflation(organizational), 1945, 0.35).
narrative_ontology:measurement(rbio_grid_22, rbio_practice_norm_complex__liberal_institutional_reading, stakes_inflation(organizational), 2024, 0.58).
narrative_ontology:measurement(rbio_grid_23, rbio_practice_norm_complex__liberal_institutional_reading, stakes_inflation(structural), 1945, 0.28).
narrative_ontology:measurement(rbio_grid_24, rbio_practice_norm_complex__liberal_institutional_reading, stakes_inflation(structural), 2024, 0.52).
narrative_ontology:measurement(rbio_grid_25, rbio_practice_norm_complex__liberal_institutional_reading, suppression(class), 1945, 0.42).
narrative_ontology:measurement(rbio_grid_26, rbio_practice_norm_complex__liberal_institutional_reading, suppression(class), 2024, 0.62).
narrative_ontology:measurement(rbio_grid_27, rbio_practice_norm_complex__liberal_institutional_reading, suppression(individual), 1945, 0.35).
narrative_ontology:measurement(rbio_grid_28, rbio_practice_norm_complex__liberal_institutional_reading, suppression(individual), 2024, 0.58).
narrative_ontology:measurement(rbio_grid_29, rbio_practice_norm_complex__liberal_institutional_reading, suppression(organizational), 1945, 0.45).
narrative_ontology:measurement(rbio_grid_30, rbio_practice_norm_complex__liberal_institutional_reading, suppression(organizational), 2024, 0.68).
narrative_ontology:measurement(rbio_grid_31, rbio_practice_norm_complex__liberal_institutional_reading, suppression(structural), 1945, 0.38).
narrative_ontology:measurement(rbio_grid_32, rbio_practice_norm_complex__liberal_institutional_reading, suppression(structural), 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__liberal_institutional_reading, 0.12).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, unsc_permanent_member_veto_power).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, economic_sanctions_regimes).

% DUAL FORMULATION NOTE:
% The RBIO kernel decomposes into three structurally distinct constraints based on competing readings of whether norms are universal/consensual/revisable (liberal reading), frozen/un-amendable/hegemonic (hegemonic reading), or illegitimate when superseding state sovereignty (sovereignty reading). The three readings have different ε values, different beneficiary/victim structures, and different classification paths. They are linked in network.affects_constraints; each story must name the other two as competing interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
