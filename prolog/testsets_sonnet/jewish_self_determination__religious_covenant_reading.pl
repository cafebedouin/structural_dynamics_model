% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Divine Covenant as Ground of Territorial Sovereignty
 *   domain: political philosophy / religious nationalism / territorial conflict
 *
 * SUMMARY:
 *   This story instantiates the religious_covenant_reading of the contested
 *   jewish_self_determination kernel: the claim that Jewish sovereignty over
 *   the land is grounded in divine covenant, rendering territorial retention
 *   a religious obligation that is not negotiable within secular political
 *   frameworks. Within the reading's own theological premises, the claim
 *   behaves like a mountain — the covenant is treated as unconditional and
 *   outside human authorization, subject to zero degrees of freedom for the
 *   believer. But the reading does not operate in a vacuum: it is deployed as
 *   political input into a contested state with real land, real residents,
 *   and real competing legal claims. Once the covenant claim enters that
 *   arena — subsidizing settlement, structuring coalition politics,
 *   foreclosing land-for-peace negotiation — its operational form is a
 *   tangled_rope: it coordinates a religious-nationalist movement's identity
 *   and mobilization (genuine coordination function) while extracting
 *   negotiating flexibility, security, and land access from Palestinian
 *   residents and from secular Israeli political actors who do not share or
 *   accept the theological premise. This is the ε-invariance boundary named
 *   in the source material: measured purely as private theological
 *   conviction, ε is near-zero; measured as the doctrine's operative
 *   political effect on territorial disposition, ε is high. This story
 *   authors the latter — the doctrine as it operates when yoked to state
 *   power and settlement policy — because that is the sense in which it
 *   functions as a constraint on other agents' lives, not merely as a belief
 *   a person holds. The other four readings of the kernel
 *   (liberal_nationalist, indigenous_return, settler_colonial, diasporist)
 *   are separate constraints, not alternate measurements of this one; each
 *   has its own beneficiary/victim structure and its own ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.72).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.68).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine Covenant as Ground of Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political philosophy / religious nationalism / territorial conflict").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '2995945f-5da1-4e78-89e2-60ab22114268').
narrative_ontology:cs_kernel_codification('2995945f-5da1-4e78-89e2-60ab22114268', fixed_text).
narrative_ontology:cs_authority_grounding('2995945f-5da1-4e78-89e2-60ab22114268', lineage).
narrative_ontology:cs_interpretation_layer_present('2995945f-5da1-4e78-89e2-60ab22114268').
narrative_ontology:cs_reading_relation('2995945f-5da1-4e78-89e2-60ab22114268', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('2995945f-5da1-4e78-89e2-60ab22114268', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('2995945f-5da1-4e78-89e2-60ab22114268', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('2995945f-5da1-4e78-89e2-60ab22114268', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('2995945f-5da1-4e78-89e2-60ab22114268', foundational, territorial_sovereignty_as_unconditional_divine_command).
narrative_ontology:cs_axiom_status(territorial_sovereignty_as_unconditional_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('2995945f-5da1-4e78-89e2-60ab22114268', territorial_sovereignty_as_unconditional_divine_command, theological).
narrative_ontology:cs_axiom('2995945f-5da1-4e78-89e2-60ab22114268', foundational, religious_obligation_independent_of_secular_political_authorization).
narrative_ontology:cs_axiom_status(religious_obligation_independent_of_secular_political_authorization, holdable).
narrative_ontology:cs_axiom_grounding('2995945f-5da1-4e78-89e2-60ab22114268', religious_obligation_independent_of_secular_political_authorization, theological).
narrative_ontology:cs_axiom('2995945f-5da1-4e78-89e2-60ab22114268', secondary, settlement_of_the_land_as_binding_commandment).
narrative_ontology:cs_axiom_status(settlement_of_the_land_as_binding_commandment, holdable).
narrative_ontology:cs_axiom_grounding('2995945f-5da1-4e78-89e2-60ab22114268', settlement_of_the_land_as_binding_commandment, conventional).
narrative_ontology:cs_reference_frame('2995945f-5da1-4e78-89e2-60ab22114268', unconditional_abrahamic_land_grant).
narrative_ontology:cs_drift_state('2995945f-5da1-4e78-89e2-60ab22114268', post_1967_settlement_expansion_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('2995945f-5da1-4e78-89e2-60ab22114268', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise_institutions).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_nationalist_political_parties).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_contested_territories).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_and_liberal_israeli_political_factions).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, divine_covenant_as_title).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, religious_obligation_of_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads Genesis and Deuteronomy covenant texts as an unconditional, binding land grant, and organizes settlement, education, and political mobilization around treating territorial retention as religious commandment (mitzvat yishuv ha'aretz). Sets the interpretive and political agenda for how the covenant claim is operationalized into settlement policy and resistance to territorial concession.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary).

% Receives state subsidies, land allocations, and legal protection premised substantially on the religious-obligation framing of settlement in the West Bank and formerly other territories. Institutional survival and expansion are directly tied to the covenant claim remaining politically and legally potent; a negotiated territorial compromise threatens the institution's core mission.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise_institutions, beneficiary,
    institutional, generational, constrained, regional).

% Convert the covenant claim into coalition leverage, ministerial portfolios, and budgetary allocation for settlement infrastructure. Can trade coalition support for policy concessions; their exit option is political (join or withhold from government) rather than doctrinal — they benefit from the claim's persistence without bearing its costs.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_nationalist_political_parties, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_nationalist_political_parties, agenda_setter).

% The body of diplomatic, legal, and land-for-peace mechanisms (UN resolutions, Oslo-framework negotiation, land-swap proposals) that depends on territory being treated as a negotiable political asset. Every negotiation round is destabilized when a domestic religious-nationalist bloc treats the same territory as non-negotiable divine patrimony; the framework absorbs the cost of the covenant claim's foreclosure effect without being able to bargain against it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework).

% Live under expanding settlement, land expropriation, and permit regimes justified in part by the religious-obligation reading of territorial retention. Have no standing within the theological framework that grounds the claim against them, and no secular negotiating partner able to override it when it holds political veto power domestically.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_contested_territories, payer,
    powerless, biographical, trapped, local).

% Seek territorial compromise as a matter of security, demography, or international legitimacy, but must build governing coalitions that frequently require religious-nationalist partners whose participation is conditioned on non-negotiation of covenant territory. Pay a political cost (coalition constraint, policy paralysis) for a claim they do not hold and cannot theologically contest on its own terms.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_and_liberal_israeli_political_factions, payer,
    organized, biographical, constrained, national).

% Assess the claim's compatibility with international law (occupation, self-determination, annexation norms) from outside both the theological and the domestic political frames. Their assessments circulate as leverage for other readings of the kernel but do not bind the religious-covenant reading's internal logic.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_legal_and_diplomatic_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, settlement_enterprise_institutions).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within religious Zionism, the covenant reading coordinates a dispersed, ideologically motivated movement around a single settlement and land-retention project, providing doctrinal certainty that substitutes for negotiated political consensus and sustains multi-generational commitment to contested territory.
% TRANSFER_FUNCTION: Moves land, state subsidy, security infrastructure, and political veto power toward the religious-nationalist settlement project, and moves negotiating flexibility, physical security, and property access away from Palestinian residents and away from secular political actors seeking territorial compromise.
% ABSENT_VOICES: Palestinian residents of the contested territories have no standing within the theological frame that grounds the claim against their land and residency; they are structurally excluded from a debate conducted in covenantal terms they do not accept and cannot contest on its own premises. Secular Israeli negotiators are present but structurally overruled whenever coalition dependence on religious-nationalist parties is activated.
% DISAPPEARANCE_RATIONALE: Religious Zionist adherents would say the world does not rearrange because the covenant is true regardless of political recognition — divine obligation persists whether or not any human institution enforces it. Secular negotiators, international observers, and Palestinian residents would say the world rearranges substantially: settlement expansion loses its most politically potent justification, coalition constraints ease, and land-for-peace frameworks regain room to operate. The verdict is genuinely contested because the two sides dispute whether the claim's political force is contingent on the movement's mobilization (in which case it would vanish with the movement) or ontologically prior to it (in which case its disappearance from politics would not touch its truth).
% FOUNDING_PROBLEM: The founding problem, as the religious-covenant reading states it, is not political but theological: the covenant in the Torah establishes an unbroken, unconditional divine promise of the land to the Jewish people, and settlement of that land is framed as commanded religious obligation rather than a solution to any contingent political problem.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist rabbinic authorities and settlement movement leadership attest the covenant obligation is permanently live and unconditional. Outside the benefiting parties, secular Israeli historians and legal scholars, Palestinian residents, and international law bodies attest that whatever the text's theological status, its operational function in this period is to foreclose negotiated compromise and legitimize a specific settlement policy — a political use of the doctrine, not merely its private religious observance. No corroboration from outside the religious-nationalist movement affirms the doctrine as a description of enforceable political entitlement; corroboration for that specific claim comes only from within the benefiting camp.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.72 by interval end: the doctrine's chief effect at the political level is to transfer land, subsidy, and negotiating leverage toward the settlement enterprise while removing negotiating room from Palestinian residents and secular Israeli political actors. Suppression sits at 0.68 — coercive in the specific sense that dissenting secular-negotiation efforts are foreclosed by coalition dependency rather than eliminated by force; the doctrine does not require physical coercion of believers, but its political operationalization requires the state's enforcement apparatus (permits, expropriation law, military administration in the territories) to hold. Theater ratio is moderate (0.4): substantial genuine religious and communal function coexists with an increasing share of the movement's activity oriented toward political maintenance of legal and demographic facts on the ground. Accessibility collapse (0.6) and resistance (0.75) reflect that alternatives to the covenant-claim political program persist and are actively pursued by the excluded and payer stakeholders — this is not a mountain in its operative political form, whatever theological absoluteness it claims internally.
 *
 * PERSPECTIVAL GAP:
 *   From the religious_zionist_movement seat, the claim computes as something close to mountain-immunity: it is prior to politics, true regardless of enforcement, indifferent to who administers it. From the palestinian_residents_of_contested_territories and secular_territorial_negotiation_framework seats, the same claim computes as an actively enforced extraction mechanism requiring state subsidy, land law, and security infrastructure to hold. The engine is expected to compute divergent per-seat types from this same structural data — that divergence is the analytical point of authoring the covenant claim as an operationalized political constraint rather than a pure theological proposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious_zionist_movement, settlement_enterprise_institutions, and religious_nationalist_political_parties are declared beneficiaries because state resources, legal protections, and political leverage flow to them under the covenant framing; their directionality sits near the beneficiary end, with identity-locked or organized exit reflecting how tightly the claim is fused to movement identity and institutional survival. Palestinian residents and secular political factions are declared victims because negotiating flexibility, land access, and physical security are removed from them by the same mechanism; their directionality sits near the target end, amplified by trapped or constrained exit options and, for residents, local scope under military/administrative control. The secular_territorial_negotiation_framework is marked as a non-agent payer (a framework, not an actor) to keep the beneficiary/victim arrays populated with real actors while still registering the structural cost to the negotiation apparatus itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem interview surfaces the mandatrophy question directly: religious authorities attest the covenant obligation is permanently live (status: contested, but the movement's own attestation is 'live' without qualification), while corroboration from outside the benefiting camp treats the doctrine's operative political function — as opposed to its private theological content — as serving present-day settlement and coalition politics rather than an unresolved genealogical problem. Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: labeling it snare would erase the genuine coordination function it performs for the religious-nationalist community's identity and continuity; labeling it mountain (accepting the movement's own theological self-description as the operative political fact) would erase the identifiable beneficiaries and victims the source material specifies. Tangled_rope holds both facts in view at once.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_versus_operationalized_epsilon,
    'Is the correct ε for this constraint the near-zero value it would carry as private theological conviction, or the high value it carries once operationalized into state settlement policy and coalition politics?',
    'Track whether the covenant claim continues to generate real-world land allocation, subsidy, and negotiation-foreclosure effects independent of which political coalition governs; if the effects persist only when the doctrine is politically enforced, ε is properly measured at the operational level authored here, not the private-belief level.',
    'If resolved toward the private-belief reading, this constraint would need to be re-scoped as a much lower-ε mountain-like belief system with no beneficiaries/victims; if resolved toward the operational reading (as authored here), tangled_rope with high ε is the accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_versus_operationalized_epsilon, conceptual, 'Whether ε is properly measured at the theological or the operationalized-political level.').

omega_variable(
    covenant_claim_natural_law_or_constructed_political_instrument,
    'Is the covenant claim best understood as a genuine, naturally-emerging religious conviction independent of political interest, or as a claim substantially shaped and sustained by the material interests of the settlement enterprise and religious-nationalist parties that benefit from it?',
    'Historical and sociological analysis of how the doctrine''s political salience has tracked settlement expansion, funding flows, and electoral incentives versus its independent theological lineage predating the modern state.',
    'If the claim''s political potency substantially tracks material beneficiary interest rather than independent theological development, this strengthens the tangled_rope reading and weakens any claim that the doctrine''s political force is theologically self-standing (mountain-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_claim_natural_law_or_constructed_political_instrument, empirical, 'Whether the covenant claim''s political operation is independently religious or materially interest-driven.').

omega_variable(
    kernel_framing_choice_documented,
    'Does the choice to author this reading at the operationalized-political level (rather than the pure-theology level) constitute an implicit framing decision that could itself be contested by adherents who hold the claim as purely private conviction with no political entailment?',
    'Compare against a hypothetical alternative story authored strictly at the private-conviction level (no beneficiaries, no victims, mountain claim, near-zero ε) and note which observable evidence (settlement policy outcomes, coalition dynamics, land law) justifies preferring the operationalized framing for THIS story.',
    'Confirms the ε-invariance discipline: the two framings are different constraints, not two measurements of one; this story deliberately selects the operationalized framing because that is the sense in which the doctrine functions as a constraint on other agents, consistent with the expected structural delta specified for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice_documented, conceptual, 'Whether the operationalized framing versus a pure-conviction framing constitutes two distinct constraints (it does, per ε-invariance).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__religious_covenant_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__religious_covenant_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__religious_covenant_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__religious_covenant_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(jewi_tr_t55, jewish_self_determination__religious_covenant_reading, theater_ratio, 55, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__religious_covenant_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__religious_covenant_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__religious_covenant_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__religious_covenant_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(jewi_be_t55, jewish_self_determination__religious_covenant_reading, base_extractiveness, 55, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__religious_covenant_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__religious_covenant_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__religious_covenant_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__religious_covenant_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(jewi_su_t55, jewish_self_determination__religious_covenant_reading, suppression_requirement, 55, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five siblings decomposing the natural-language concept 'the basis of Jewish self-determination claims to the land' per the ε-invariance principle. Each sibling reading (liberal_nationalist, indigenous_return, settler_colonial, diasporist, and this religious_covenant reading) has a distinct ε, distinct beneficiary/victim structure, and distinct claimed_type, because each grounds the territorial claim in a structurally different warrant (national self-determination theory, indigenous continuity, colonial critique, diaspora political theory, and divine covenant respectively). This reading is distinguished among the five by treating the claim as categorically outside secular negotiability, which is the specific structural feature that produces its foreclosure relationship with the liberal_nationalist and indigenous_return readings even where those readings might reach overlapping territorial conclusions by negotiable means.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
