% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty and Humanitarian Intervention Authority
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The conditional sovereignty reading reframes the Westphalian principle
 *   that states possess inviolable territorial authority. Under this reading,
 *   a state's right to non-interference is not absolute but conditional on
 *   its protection of populations from mass atrocities. When a state fails
 *   this protection threshold, the international community (through
 *   authorized coalitions and institutions) gains legitimate authority to
 *   intervene militarily, impose sanctions, or administer territory. This
 *   reading emerged from the post-Cold War era (Rwanda, Kosovo) and competes
 *   with two sibling readings: absolute non-intervention (sovereignty is
 *   categorical regardless of internal conduct) and graded sovereignty
 *   (territorial authority exists on a spectrum with calibrated intervention
 *   thresholds). The constraint story tracks this specific reading's
 *   operation—how it allocates authority, who benefits from its invocation,
 *   and what costs it imposes on states and populations. CLAIM AND METRICS
 *   ARE INDEPENDENT: the constraint is CLAIMED as tangled rope (genuine
 *   coordination function—atrocity protection—paired with asymmetric
 *   extraction—authority transfer to external institutions). Metrics are
 *   authored as descriptively true of the doctrine's actual operation:
 *   extractiveness is high (0.68) because the authority transfer is
 *   substantial and benefits the intervening coalition; suppression is high
 *   (0.71) because opposing sovereignties must be actively overridden;
 *   theater is moderate (0.42) because the humanitarian justification is
 *   partly genuine and partly performative cover for geopolitical
 *   positioning. The engine measures the divergence between claim and
 *   metrics; no reconciliation is attempted.
 *
 * KEY AGENTS:
 *   - Humanitarian intervention coalitions (Western states, NATO): agenda-setters with institutional power and arbitrage-level exit; they frame and invoke the conditionality doctrine to authorize action they already intend to take.
 *   - International governance institutions (UN, ICC, UNHCR): beneficiaries gaining expanded mandate and authority; they mediate and administer the doctrine's application.
 *   - Atrocity-affected populations (powerless, trapped): structurally dependent on the doctrine for protection, but also bearing costs of intervention and stigmatization.
 *   - States under scrutiny (moderate power, constrained exit): vulnerable to having their sovereignty declared conditional; their exit consists only of suppression or capitulation.
 *   - Global South non-aligned states (organized, constrained): excluded from effective control of the doctrine despite UN voice; structurally overridden by Security Council power dynamics.
 *   - Regimes committing atrocities (moderate power, trapped): direct targets of the doctrine with no exit except military defeat or regime change.
 *   - Alternative sovereignty framers (powerful, trapped): advocates for absolute non-intervention or graded sovereignty lack institutional dominance and are unable to set the agenda.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.71).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty and Humanitarian Intervention Authority").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, 'cc867655-682e-4be6-b126-d0831268c981').
narrative_ontology:cs_kernel_codification('cc867655-682e-4be6-b126-d0831268c981', fixed_text).
narrative_ontology:cs_authority_grounding('cc867655-682e-4be6-b126-d0831268c981', extraction).
narrative_ontology:cs_interpretation_layer_present('cc867655-682e-4be6-b126-d0831268c981').
narrative_ontology:cs_reading_relation('cc867655-682e-4be6-b126-d0831268c981', westphalia_sovereignty__absolute_non_intervention, coexists_with).
narrative_ontology:cs_reading_relation('cc867655-682e-4be6-b126-d0831268c981', westphalia_sovereignty__graded_sovereignty, influences).
narrative_ontology:cs_axiom('cc867655-682e-4be6-b126-d0831268c981', foundational, atrocity_forfeits_sovereignty).
narrative_ontology:cs_axiom_status(atrocity_forfeits_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('cc867655-682e-4be6-b126-d0831268c981', atrocity_forfeits_sovereignty, deontological).
narrative_ontology:cs_axiom('cc867655-682e-4be6-b126-d0831268c981', foundational, humanitarian_intervention_legitimacy).
narrative_ontology:cs_axiom_status(humanitarian_intervention_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cc867655-682e-4be6-b126-d0831268c981', humanitarian_intervention_legitimacy, deontological).
narrative_ontology:cs_axiom('cc867655-682e-4be6-b126-d0831268c981', secondary, international_community_adjudicative_authority).
narrative_ontology:cs_axiom_status(international_community_adjudicative_authority, holdable).
narrative_ontology:cs_axiom_grounding('cc867655-682e-4be6-b126-d0831268c981', international_community_adjudicative_authority, conventional).
narrative_ontology:cs_reference_frame('cc867655-682e-4be6-b126-d0831268c981', sovereign_state_non_interference).
narrative_ontology:cs_drift_state('cc867655-682e-4be6-b126-d0831268c981', post_cold_war_intervention_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cc867655-682e-4be6-b126-d0831268c981', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, international_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, states_under_scrutiny).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, global_south_non_aligned_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, regimes_committing_atrocities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coalitions of Western states (US, EU, NATO members) and coalitions of the willing that invoke the conditional sovereignty doctrine to justify military and humanitarian interventions. They frame interventions as protection of universal human rights and set the terms under which sovereignty is judged conditional. They gain geopolitical positioning, resource allocation authority, and legitimacy for actions they independently intend to take. They can arbitrage between conflict zones and claim new atrocities justify intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, agenda_setter,
    institutional, generational, arbitrage, global).

% UN Security Council, International Criminal Court, UNHCR, OCHA, regional bodies (African Union, European Court of Human Rights), and the expansive bureaucracy of international governance. They gain expanded institutional mandate, enlarged budgets, expanded staffing, and enhanced authority to assess state conduct, declare sovereignty conditional, and authorize or oversee interventions. Each new atrocity declaration expands their scope and legitimacy. They mediate the application of the conditional sovereignty framework.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_governance_institutions, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, international_governance_institutions, agenda_setter).

% Populations living under regimes committing genocide, crimes against humanity, or war crimes. They benefit from the doctrine's framing that their protection is a legitimate international concern and that external intervention is justified by their suffering. They also bear substantial costs: military interventions often involve collateral civilian casualties and displacement, the international attention stigmatizes their entire nation-state as 'failed' or 'criminal,' the doctrine's selective application means many atrocities receive no intervention despite causing equal suffering, and post-intervention governance often involves external administration that limits their political agency.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations, payer).

% States whose internal conduct is continuously assessed against atrocity thresholds by external institutions. They bear the costs of conditionality: weakened bargaining position in international forums, loss of authority over territory if conditionality is invoked, vulnerability to military intervention, external sanctions, ICC prosecution of state officials, and international administration of territory. Their exit options are constrained to internal suppression or capitulation; they cannot exit the state system.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, states_under_scrutiny, payer,
    moderate, generational, constrained, national).

% Nations outside the Western intervention coalition (BRICS, Non-Aligned Movement members, African Union members) that oppose the conditional sovereignty doctrine on grounds of hypocrisy (atrocities by Western states are not addressed), unequal application (small/weak states are heavily scrutinized; powerful states are protected by Security Council veto), and erosion of non-interference principles that have historically protected their own sovereignty. They have voice in UN General Assembly and some forums but are structurally overridden by Security Council dominance of Western states. They bear costs through selective application of the doctrine against their interests.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_south_non_aligned_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, global_south_non_aligned_states, excluded).

% States whose security forces carry out mass violence (genocide, crimes against humanity, war crimes). They face direct military intervention threat, international sanctions, seizure of assets, ICC prosecution of state officials, territorial loss, and loss of diplomatic legitimacy under the conditional sovereignty framework. Their exit options are trapped: they cannot leave the territory they govern, and they cannot exit the state system. Their only options are internal military suppression or capitulation to intervention demands.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, regimes_committing_atrocities, payer,
    moderate, biographical, trapped, national).

% States, legal scholars, and movements that advocate for absolute non-intervention based on classical Westphalian sovereignty (territorial authority is inviolable regardless of internal conduct). They argue the conditional sovereignty doctrine is inconsistently applied, undermines the non-interference principle that protects smaller states, and enables powerful states to intervene for geopolitical reasons while claiming humanitarian motives. They are structurally excluded from determining when the conditionality doctrine applies; their voice is heard in legal forums but overridden by institutional dominance of intervention coalitions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, absolute_non_intervention_advocates, excluded,
    powerful, generational, trapped, global).

% International legal scholars and some states that advocate for graded sovereignty (territorial authority exists on a spectrum from full to nominal, with intervention calibrated to state capacity deficits rather than atrocity thresholds alone). They argue conditional responsibility bundles atrocity response with capacity-gap intervention, conflating humanitarian protection with development imposition. They are structurally excluded from setting the agenda but influence how intervention is framed (increasingly, conditional responsibility doctrine incorporates capacity-gap language).
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, graded_sovereignty_advocates, excluded,
    organized, generational, constrained, global).

% Academic lawyers, NGO analysts, think-tank researchers, and professional observers who document the conditional sovereignty doctrine's operation, measure selective application across cases, track intervention outcomes, compare human rights improvements vs. displacement and violence, and assess whether the doctrine's humanitarian framing matches its actual effects on protected populations. They produce analytical scrutiny that shapes discourse but do not control implementation.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global norm that protection of populations from mass atrocity is a legitimate international concern, authorizing coordinated humanitarian intervention rather than isolated state action. Solves the coordination problem of atrocities occurring across sovereign boundaries: absent the doctrine, each state acts unilaterally; the doctrine enables legitimized coalition action under international legal cover.
% TRANSFER_FUNCTION: Transfers adjudicative authority over state legitimacy from each state's own assessment to external institutions (UN, ICC, regional courts). Transfers the right to intervene in domestic affairs from the state (alone) to authorized coalitions of states and international bodies. Transfers the framing of atrocities from local/regional issue to global governance concern, routing resources and attention through international institutions rather than locally.
% ABSENT_VOICES: Regimes committing atrocities have no legitimate voice in the adjudication that conditions their sovereignty—the doctrine structurally excludes their account of events as irrelevant to the determination. States advocating for absolute non-intervention are present in UN forums but structurally overridden. Populations in non-atrocity conflicts are absent: the doctrine focuses attention on mass violence but not on structural poverty, inequality, or chronic state failure that affect more people.
% DISAPPEARANCE_RATIONALE: If the conditional sovereignty doctrine vanished, intervention coalitions would lose their legal cover for humanitarian military action; states would revert to classical non-intervention norms and balance-of-power calculations; international institutions would lose their expanded mandate to assess and condition state legitimacy; regimes facing atrocity accusations would regain classical sovereignty protections. The global governance architecture depends on conditionality to justify its expanded scope.
% FOUNDING_PROBLEM: Mass atrocities occurring within state borders were treated as purely domestic affairs under classical Westphalian sovereignty, leaving affected populations with no international protection mechanism and enabling perpetrators to shelter behind non-interference doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian advocates and intervention coalitions attest the problem remains live and urgent (Rwanda, Syria, Myanmar cases cited). Non-aligned states and international legal scholars attest the founding problem is selectively invoked: atrocities by powerful states (US detention abuses, Israeli conduct in Gaza, Russian actions in Ukraine) do not trigger the conditionality doctrine, indicating the framing serves geopolitical interest rather than universal protection. Independent analysis of intervention outcomes shows mixed results: some interventions prevent atrocities, others displace populations and create new atrocities.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and rising steeply in early periods (0.48→0.68 from t=0 to t=25) because the doctrine systematically transfers adjudicative authority from the state to external institutions, and this transfer benefits the intervening coalition and international governance bodies at the expense of vulnerable states. The measurement series shows extractiveness plateaus at t=25, indicating the doctrine has reached its operational maturity—additional application doesn't extract more authority, it redistributes it among the same beneficiaries. Suppression is high (0.71, and rising from 0.55→0.71, t=0 to t=25) because the doctrine requires active enforcement to override state resistance: intervention coalitions must maintain military readiness, sanction regimes, and sustain institutional mechanisms to suppress alternative sovereignty framings (absolute non-intervention, graded sovereignty). The plateau at t=25 reflects institutional stabilization—the enforcement machinery is mature and maintains itself. Theater ratio rises to 0.42 and plateaus, indicating that the humanitarian justification is real but not the whole story: the measurement indicates ~42% of the constraint's activity is performative (framing geopolitical positioning as protection, selective application based on strategic interest rather than atrocity severity). Accessibility collapse is moderate-high (0.64) because states cannot realistically exit the sovereignty system entirely, but they retain some negotiating room and can invoke alternative framings within UN bodies. Resistance is high (0.73) because substantial state and scholarly opposition persists; the doctrine is actively contested by non-aligned coalitions and international law communities that deny its legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (intervention coalitions, international institutions), the constraint is genuine coordination: it solves the problem of how to protect populations and authorizes legitimate humanitarian action. From the payer seats (states under scrutiny, Global South, regimes), the same structure operates as weaponized authority transfer: the doctrine's application is selective (Western states' atrocities go unaddressed), its invocation is often pretextual (framing geopolitical intervention as humanitarian), and its effects are destabilizing (populations experience intervention-induced displacement and violence alongside protection). The engine computes these per-seat divergences from the structural data: beneficiaries have low directionality (d near 0.0, extractiveness is inverted to subsidy for them), payers have high directionality (d near 1.0, extractiveness directly extracted from them). The perspectival gap is not a flaw in the story—it is the measurement the story exists to take: a constraint where the beneficiary seats and payer seats compute different types is exactly how structural extraction is detected.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanitarian intervention coalitions: d ≈ 0.1 (full beneficiary). They gain authority, legitimacy, and geopolitical positioning from the doctrine's operation. Their exit options are arbitrage-level (they can shift between conflict zones and claim new atrocities justify intervention). Power is institutional and globally distributed. They benefit from every invocation. Directional subsidy for them (negative effective extraction). International governance institutions: d ≈ 0.15 (near-beneficiary). They gain expanded mandate and resource allocation. Exit options are mobile (they can reframe their role, participate in peacekeeping, humanitarian response). Directional subsidy for them, though somewhat less than coalitions because institutional legitimacy is more fragile. Atrocity-affected populations: d ≈ 0.55 (near-symmetric, slightly extracted). They benefit from the doctrine's protection framing and international attention, but bear costs of intervention (military action, displacement, long-term governance uncertainty). Their exit options are trapped (they cannot leave their territory or exit the governance relationship the doctrine imposes). Power is powerless. They experience both coordination benefit and extraction cost. Symmetric or slightly extractive directionality. States under scrutiny: d ≈ 0.75 (strongly extracted). They lose sovereign authority, face intervention threats, bear sanctions, and have no say in the determination of whether their sovereignty is conditional. Exit options are constrained (they cannot leave the state system, can only suppress or capitulate). Power is moderate. Strongly extractive directionality. Global South non-aligned states: d ≈ 0.70 (strongly extracted). They bear the costs of the doctrine's selective application (their atrocities are ignored, their sovereignty is protected only as long as they don't oppose the intervention coalition), constrained exit (trapped in the state system), organized power (some voice in UN but structurally overridden). Strongly extractive. Regimes committing atrocities: d ≈ 0.85 (strongly target). They face military intervention, sanctions, territorial loss, prosecution. Trapped exit. Moderate power but concentrated opposition from intervention coalitions. Strongly extracted. Alternative sovereignty framers: d ≈ 0.72 (strongly extracted). They cannot effectively promote their alternative framings (absolute non-intervention, graded sovereignty) because institutional dominance is held by intervention coalitions. Trapped in the state system. Powerful individually but unable to coordinate globally. Strongly extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   The conditional sovereignty doctrine emerged from a genuine coordination problem: classical non-intervention left atrocity-affected populations without protection mechanism, and individual state humanitarian action lacked legal cover and was subject to realpolitik calculation. The coordination function is real and persists: the doctrine legitimizes collective humanitarian action, enables resource pooling through UN bodies, and creates formal mechanisms for atrocity investigation and response. However, the doctrine's mandate has become substantially detached from its founding function. The measurement series shows theater ratio rising to 0.42 and remaining elevated: the doctrine's humanitarian framing increasingly covers geopolitical positioning rather than atrocity response. The selective application (Western atrocities unaddressed, small states heavily scrutinized) indicates the mandate has inverted: instead of universal protection, the doctrine now serves to legitimize intervention in strategically interesting territories while maintaining non-interference for powerful states. The independence of the theatrical element (theater ratio ≈ 0.42) from the coordination function indicates mandatrophy is PARTIAL rather than complete—the doctrine still coordinates humanitarian response in some contexts (genuine atrocity emergencies), but the coordination function is increasingly secondary to the extraction function (authority transfer to intervening coalitions and international institutions). The constraint is classified as tangled rope (has both genuine coordination and asymmetric extraction), not snare (pure extraction cover) or piton (atrophied function maintained theatrically), because the coordination function persists even as the extraction function expands. Mandatrophy is NOT resolved: the founding problem (atrocity protection) remains contested as live/dead (some atrocities are prevented, others are ignored; protection is real in some contexts and pretextual in others). The divergence between the coordination and extraction functions is irreducible within this reading—resolving it would require either fully subordinating extraction to coordination (strict non-selective application) or admitting the doctrine is pure geopolitical authority transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditional_vs_constructed,
    'Is the conditionality of sovereignty a genuine structural principle of legitimate authority, or a constructed doctrine that benefits intervening powers?',
    'Comparative analysis of intervention patterns: universal application across all atrocity cases regardless of intervener interest (structural principle) vs. selective application based on geopolitical utility (constructed doctrine). Textual analysis of UN and ICC decision records showing whether atrocity severity or strategic interest predicts intervention authorization.',
    'If genuinely structural, the constraint is coordination with asymmetric extraction (tangled rope). If constructed, it is snare (pure extraction cover). The classification hinges on whether the doctrine''s operation matches its humanitarian framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditional_vs_constructed, empirical, 'Whether conditionality is principle or constructed authority.').

omega_variable(
    atrocity_definition_ambiguity,
    'Who defines what constitutes ''mass atrocity'' sufficient to trigger conditionality? Is the definition objective, or does it shift based on geopolitical interest?',
    'Institutional audit of atrocity thresholds applied by UN, ICC, and regional bodies: are thresholds fixed, or do they vary by case? Do similar violence levels by different perpetrators receive equivalent atrocity determinations? Do Western state atrocities receive same scrutiny as non-Western state atrocities?',
    'If definition is objective and consistently applied, the conditionality doctrine is legitimated by principle. If definition shifts strategically, the doctrine''s authority is hollow—it operates as a discretionary power transfer to intervening coalitions, not as a genuine coordination principle. This feeds the mandatrophy analysis: if atrocity definition is strategic, the founding problem (protection) is decoupled from the actual operation (authority positioning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_definition_ambiguity, empirical, 'Whether atrocity thresholds are objective or strategically defined.').

omega_variable(
    intervention_efficacy_gap,
    'When the conditional sovereignty doctrine authorizes intervention, do interventions actually reduce atrocities and protect populations, or do they often create new displacement and violence?',
    'Longitudinal study of post-intervention outcomes in cases where conditionality doctrine was invoked (Kosovo, Iraq, Libya, Syria). Compare atrocity prevalence pre-intervention, during intervention, and post-intervention. Measure displacement, civilian casualties, and long-term state capacity in post-intervention vs. non-intervention contexts.',
    'If interventions systematically reduce atrocities and improve protection, the coordination function is validated. If interventions often worsen outcomes, the founding problem justification is undermined—the constraint persists not because it solves atrocity protection but because it benefits the intervening coalition. This would push classification toward piton (atrophied coordination, maintained theatrically) rather than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_efficacy_gap, empirical, 'Whether intervention actually protects populations or creates new harm.').

omega_variable(
    western_power_asymmetry,
    'Is the lack of intervention in Western state atrocities (detention abuses, civilian bombing, occupation conduct) evidence that conditionality doctrine is genuinely about protection, or evidence that it is a tool of power asymmetry?',
    'Historical and institutional analysis: would NATO members accept intervention authorization by the ICC or UN for conduct equivalent to that of non-Western states? Would Security Council veto power be used to block such authorization? Would non-aligned states voice intervention demands that are rejected, or do they accept Western atrocity immunity as part of the great-power system?',
    'If Western states would accept conditionality applied equally, the doctrine is principle-based (tangled rope with asymmetric extraction that could be remedied by equal application). If Western states use veto power and institutional dominance to maintain atrocity immunity, the doctrine is pure power asymmetry (snare), and the humanitarian framing is entirely performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(western_power_asymmetry, empirical, 'Whether conditionality is principle or Western power asymmetry.').

omega_variable(
    kernel_reading_boundary,
    'Does the conditional responsibility reading genuinely represent a single coherent interpretation of the Westphalia kernel, or does it bundle two distinct claims: (1) atrocities are a legitimate international concern, and (2) the international community has authority to intervene to stop them?',
    'Philosophical and legal analysis of the reading''s foundational premises. Can (1) be accepted without accepting (2)? Can international concern be expressed through non-military mechanisms (investigation, prosecution, sanctions) without armed intervention? Would a state accept that atrocities are international concern but reject that this concern legitimates external military intervention?',
    'If the reading bundles distinct claims, it may not be a single coherent interpretation of the kernel but rather two separable constraints: (A) atrocities as international concern (weaker, more consensual), and (B) external military intervention authority (stronger, more contested). The current story treats them as unified; decomposition would yield two separate constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether conditional responsibility is a single reading or two bundled claims.').

omega_variable(
    alternative_reading_suppression,
    'Is the near-absence of absolute non-intervention framing in contemporary international law discourse evidence that the reading is logically foreclosed by conditional responsibility, or evidence that it is suppressed by institutional power?',
    'Archival and citation analysis: does absolute non-intervention appear in ICJ rulings, UN General Assembly resolutions, and academic international law? Is it cited and refuted, or simply absent from institutional discourse? Do non-aligned states and smaller powers invoke it, but have their voice overridden? Is there active institutional effort to delegitimize it (framing it as enabling genocide) vs. simply not discussing it?',
    'If absolute non-intervention is logically foreclosed (conditional responsibility truly makes it incoherent), the reading_relations should declare ''forecloses''. If it is suppressed by power asymmetry (non-aligned states invoke it but are overridden), the reading_relations should declare ''coexists_with'' (it remains a live position held by other parties despite not appearing in official discourse). This affects the kernel''s structural stability: foreclosure indicates mature reading dominance; suppression indicates fragile dominance dependent on power maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_suppression, empirical, 'Whether alternative readings are foreclosed or suppressed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(west_tr_t0, projected).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__conditional_responsibility, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(west_tr_t5, observed).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__conditional_responsibility, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(west_tr_t10, observed).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__conditional_responsibility, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(west_tr_t15, observed).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__conditional_responsibility, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(west_tr_t20, observed).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__conditional_responsibility, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(west_tr_t25, observed).
narrative_ontology:measurement(west_tr_t35, westphalia_sovereignty__conditional_responsibility, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(west_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(west_be_t0, projected).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(west_be_t5, observed).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(west_be_t10, observed).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(west_be_t15, observed).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(west_be_t20, observed).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(west_be_t25, observed).
narrative_ontology:measurement(west_be_t35, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(west_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(west_su_t0, projected).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(west_su_t5, observed).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(west_su_t10, observed).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(west_su_t15, observed).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(west_su_t20, observed).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(west_su_t25, observed).
narrative_ontology:measurement(west_su_t35, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(west_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__conditional_responsibility, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, international_court_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-part kernel split: the Westphalia sovereignty kernel is instantiated by three distinct readings—conditional responsibility (this story), absolute non-intervention (sibling constraint), and graded sovereignty (sibling constraint). Each reading has a different ε, different beneficiary/victim structure, and different classification. They are linked as siblings of the same kernel, not as alternative measurements of a single constraint. The ε for this reading is 0.68 (high extraction from vulnerable states, moderate coordination benefit). The ε for absolute non-intervention would be ~0.15 (protection of state autonomy, minimal extraction, pure coordination). The ε for graded sovereignty would be ~0.55 (moderate extraction, genuine capacity-based coordination). The three readings are structurally incommensurable—changing readings changes the referent constraint, not the measurement basis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
