% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony Reading of Territorial Legitimacy — Continuous Habitation, Displacement, and Right of Return
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the Palestinian autochthony reading of the
 *   contested territorial-legitimacy kernel: legitimacy is grounded in
 *   continuous habitation predating 1948, the displacement of that year and
 *   1967 is read as an ongoing, unremedied injustice rather than a closed
 *   historical event, and the right of return is treated as a live,
 *   non-negotiable individual and collective entitlement under international
 *   law (anchored in UNGA 194) rather than a bargaining chip subject to
 *   compromise. Under this reading, the standing arrangement — Israeli
 *   territorial and demographic control administered without recognizing
 *   return, alongside continuing settlement expansion — is assessed as
 *   substantially extractive: land, residency, and political recognition flow
 *   toward the Israeli state and settlement institutions while refugees and
 *   their descendants bear compounding, generational costs. This is a
 *   distinct constraint from the sibling readings: the zionist_refuge_reading
 *   authors a low-ε, legitimacy-affirming account of the same territorial
 *   history (historical persecution, divine promise, UN partition
 *   acceptance), and the two_state_coexistence_reading authors a moderate-ε
 *   account treating 1967 lines as a workable compromise framework with dual
 *   legitimacy. All three share the underlying kernel (who holds title to the
 *   land and on what warrant) but diverge sharply in ε, beneficiary/victim
 *   structure, and classification, exactly as the ε-invariance principle
 *   requires — this is not the same constraint measured three ways, it is
 *   three constraints.
 *
 * KEY AGENTS:
 *   - palestinian_refugees_1948: primary target (powerless/trapped) — bears the founding deprivation and its multi-generational compounding
 *   - israeli_state_apparatus: primary beneficiary and agenda-setter (institutional/arbitrage) — controls the Law of Return, border administration, and adjudication terms
 *   - settlement_enterprise_institutions: secondary beneficiary (institutional/arbitrage) — consumes the land base the claim would be exercised over
 *   - international_mediators: analytical observer (institutional/analytical) — records the claim without enforcement leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.81).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.86).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony Reading of Territorial Legitimacy — Continuous Habitation, Displacement, and Right of Return").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '7678417e-2d86-4860-be0e-7c813a9744bb').
narrative_ontology:cs_kernel_codification('7678417e-2d86-4860-be0e-7c813a9744bb', distributed).
narrative_ontology:cs_authority_grounding('7678417e-2d86-4860-be0e-7c813a9744bb', distributed).
narrative_ontology:cs_reading_relation('7678417e-2d86-4860-be0e-7c813a9744bb', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('7678417e-2d86-4860-be0e-7c813a9744bb', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('7678417e-2d86-4860-be0e-7c813a9744bb', foundational, continuous_habitation_grounds_unextinguished_title).
narrative_ontology:cs_axiom_status(continuous_habitation_grounds_unextinguished_title, holdable).
narrative_ontology:cs_axiom_grounding('7678417e-2d86-4860-be0e-7c813a9744bb', continuous_habitation_grounds_unextinguished_title, deontological).
narrative_ontology:cs_axiom('7678417e-2d86-4860-be0e-7c813a9744bb', foundational, displacement_does_not_extinguish_return_right).
narrative_ontology:cs_axiom_status(displacement_does_not_extinguish_return_right, holdable).
narrative_ontology:cs_axiom_grounding('7678417e-2d86-4860-be0e-7c813a9744bb', displacement_does_not_extinguish_return_right, conventional).
narrative_ontology:cs_reference_frame('7678417e-2d86-4860-be0e-7c813a9744bb', pre_1948_demographic_and_land_tenure_baseline).
narrative_ontology:cs_drift_state('7678417e-2d86-4860-be0e-7c813a9744bb', post_oslo_settlement_expansion_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7678417e-2d86-4860-be0e-7c813a9744bb', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, settlement_enterprise_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1967).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora_descendants).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, continuous_habitation_grounds_title).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, displacement_does_not_extinguish_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced from villages and towns in 1948 (the Nakba) and their descendants, now numbering in the millions across refugee camps in Lebanon, Syria, Jordan, and Gaza. Hold UNRWA registration and, in this reading, an unextinguished individual and familial right of return under international law. Cannot return to original homes or claim property; the state that now controls that territory does not recognize the claim as actionable. Exit from refugee status is blocked both by host-country citizenship restrictions and by the non-recognition of return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1948, payer,
    powerless, civilizational, trapped, regional).

% Displaced in the 1967 war from the West Bank and Gaza into Jordan and elsewhere; a second wave layered onto the unresolved 1948 displacement. Carry a distinct but related claim; often treated administratively as a separate, lesser category with fewer return prospects even in negotiated frameworks.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1967, payer,
    powerless, generational, trapped, regional).

% Live under blockade and periodic military operations in a territory whose population is majority-refugee-descended. In this reading, their confinement is a direct continuation of 1948 displacement rather than a separate contemporary security matter. Movement in and out is controlled by external parties; there is no unilateral exit.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_residents, payer,
    powerless, immediate, trapped, local).

% Live under a mix of Palestinian Authority and Israeli military administration, with continuing settlement expansion narrowing contiguous territory available to them. In this reading, settlement growth is the ongoing operational mechanism of the 1948 displacement, not a separate post-1967 policy question. Movement between enclaves requires permits from an authority they do not elect.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_residents, payer,
    powerless, biographical, constrained, local).

% Descendants of 1948/1967 refugees now settled across the Middle East, Europe, and the Americas, often with third-country citizenship. Hold the claim in inherited, largely symbolic form — their return right is legally live in this reading but practically unexercisable; many have built lives elsewhere but the unresolved claim shapes political identity across generations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora_descendants, payer,
    moderate, generational, constrained, global).

% Administers the territory, controls border crossings, citizenship law, and the Law of Return that structurally forecloses Palestinian return while enabling Jewish immigration. In this reading, the state's founding and continued demographic policy are the mechanism that produced and sustains the displacement; the state sets the terms under which any Palestinian claim can even be adjudicated, and has no structural incentive to concede a return right that would alter its demographic character.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, beneficiary).

% Government ministries, settlement councils, and financing bodies that plan and fund West Bank settlement expansion. In this reading, their activity directly consumes the land base over which any future return or restitution claim would be exercised, converting contested territory into irreversible-seeming facts on the ground.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, settlement_enterprise_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Lebanon, Syria, and Jordan host large long-term refugee populations, often denying full citizenship to preserve the political salience of the return claim domestically and regionally. They are affected parties but are not treated as principals in most negotiation frameworks and are excluded from the bilateral tracks that would decide the refugees' fate.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, host_arab_states, excluded,
    moderate, generational, constrained, regional).

% UN bodies, the Quartet, and rotating great-power sponsors convene negotiations and issue resolutions (e.g., UNGA 194) that in this reading affirm the return right but lack an enforcement mechanism. They record and periodically restate the claim without possessing leverage to compel its resolution.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_mediators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In this reading there is minimal genuine coordination function for the standing arrangement itself — the arrangement is read as the continuation of an original displacement rather than a jointly-built solution to a shared problem. The coordination that DOES exist (armistice lines, UNRWA administration, permit systems) manages the consequences of the original act rather than solving a problem both sides created together.
% TRANSFER_FUNCTION: Land, property, and residency rights move from Palestinian residents and their descendants to the Israeli state and settlement institutions; political and legal recognition of continuous title moves toward the Israeli state; the cost of unresolved status (statelessness, camp conditions, blocked mobility) is borne by refugees and their descendants across generations.
% ABSENT_VOICES: Refugees in host states are frequently excluded from direct negotiation tracks conducted between the PA/PLO leadership and Israel or mediating powers; camp populations and diaspora communities are represented, if at all, by leadership structures they did not directly select, and their consent to any final-status compromise on return is rarely tested directly.
% DISAPPEARANCE_RATIONALE: If this legitimacy claim were to disappear — i.e., if continuous habitation and displacement ceased to ground any political or legal claim — the entire architecture of UNRWA, refugee registration, right-of-return diplomacy, and Palestinian national self-conception organized around 1948 would lose its foundation; land restitution and compensation claims would become unmoored, and Palestinian political identity itself, substantially organized around the Nakba narrative and the return demand, would require fundamental reconstruction.
% FOUNDING_PROBLEM: The 1948 war produced approximately 700,000 Palestinian refugees whose homes, land, and property came under the control of the newly declared state of Israel, with UN General Assembly Resolution 194 (1948) affirming a right of return or compensation that was never implemented.
% FOUNDING_PROBLEM_CORROBORATION: UNRWA (an international body, not a Palestinian institution) continues to register refugee descendants and document the unresolved status; independent human rights organizations (Amnesty International, B'Tselem — an Israeli organization) and UN Special Rapporteurs corroborate ongoing displacement effects (settlement expansion, movement restriction) from outside the Palestinian leadership itself. Israeli historians of the 'New Historians' school (Benny Morris, Ilan Pappé) have corroborated core facts of the 1948 displacement from within Israeli academia, though they differ sharply on its legal and moral status.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) and rising because, in this reading, each subsequent decade of settlement expansion and non-recognition of return compounds rather than resolves the original 1948 transfer of land and residency rights. Suppression is authored higher still (0.86) because the arrangement's persistence depends on active administrative and military mechanisms — permit systems, border control, non-recognition of refugee registration claims as actionable title — not on voluntary participant acceptance. Theater ratio is moderate (0.42) and rising, reflecting an authored judgment that a growing share of diplomatic and administrative activity (peace-process rhetoric, periodic UN resolutions restating 194 without enforcement) is performative relative to the underlying territorial facts, which continue to shift toward settlement consolidation regardless of the diplomatic theater layered on top. Resistance is authored very high (0.88): this reading holds that Palestinian claim-making, refugee political organization, and international advocacy constitute substantial, sustained resistance rather than passive acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state apparatus and settlement institutions are coded as structural beneficiaries: they control the instruments (Law of Return, land administration, settlement planning) that both produced and now maintain the arrangement, and their exit options are best characterized as arbitrage — they can adjust policy unilaterally without losing standing. Palestinian refugees across all cohorts are coded as targets with trapped or constrained exit: 1948 and 1967 refugees cannot exercise return or receive compensation; Gaza and West Bank residents live under externally controlled movement regimes; diaspora descendants hold the claim in inherited but practically unexercisable form. Host Arab states are excluded from principal negotiating status despite bearing real costs, which this reading treats as a structural exclusion worth flagging via the absent_voices question rather than a correction to classification (per R3, this stays commentary-grade).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification here resists two collapse failures. First, it does not treat the arrangement as a pure natural-law or settled-fact mountain (the 'nothing to see here, this is simply the current state of sovereignty' framing), because that framing would erase the beneficiary structure the reading holds to be real and ongoing. Second, it does not collapse the founding problem into 'resolved' status merely because decades have passed and institutional machinery (UNRWA, peace processes) has grown up around it — the founding_problem_status is authored as 'live' precisely because, by this reading's own lights, the 1948 deprivation was never remedied, only administered. The mismatch the schema is designed to surface — status=live paired with disappearance_verdict=world_rearranges — is coherent here, not a red flag: a live, unremedied founding problem whose disappearance would rearrange the world is exactly what a persistent, contested, high-extraction kernel reading should look like.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    return_right_legal_status_ambiguity,
    'Does UNGA Resolution 194 establish an enforceable individual legal right of return under international law, or is it a non-binding political recommendation whose ongoing invocation is itself an unresolved question of international legal interpretation?',
    'Authoritative adjudication by an international judicial body with binding jurisdiction over the question (which does not currently exist for this dispute), or durable multilateral consensus among the involved states on the resolution''s legal weight.',
    'If binding and enforceable, the extraction reading strengthens substantially — non-return becomes a continuing legal violation. If non-binding political recommendation only, the claim''s legal footing weakens even as its moral and political force may persist, which would not change this reading''s ε but would sharpen the omega for any adjudicating body.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(return_right_legal_status_ambiguity, conceptual, 'Whether the right of return is a binding legal entitlement or a non-binding political claim shapes how the extraction this reading identifies should be legally characterized.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three declared readings of the territorial_legitimacy_dual kernel (palestinian_autochthony, zionist_refuge, two_state_coexistence) genuinely incommensurable framings resting on different foundational premises about title, or do they share enough common ground (e.g., acceptance of UNGA 194''s existence, acceptance of 1948 displacement as historical fact) that a synthesis reading could in principle be constructed?',
    'Sustained comparative analysis of the axioms underlying each reading (see cs_structure.axioms across the three sibling files) to determine whether the foreclosure/coexistence/influence relations declared are stable across different adjudicating frameworks, or whether a fourth ''synthesis'' reading would itself constitute a legitimate additional kernel reading.',
    'If genuinely incommensurable, the kernel is correctly modeled as three distinct constraints with no meta-level resolution available except political negotiation. If partial common ground exists, some sibling relations currently declared as coexists_with might be better modeled as influences, and the network should be re-examined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings are truly incommensurable or admit partial synthesis is itself unresolved and shapes how the reading_relations should be interpreted going forward.').

omega_variable(
    demographic_intent_ambiguity,
    'Is the state''s non-recognition of return primarily driven by demographic-preservation policy (an intentional mechanism this reading treats as central to the extraction), or primarily by unresolved security concerns that would persist even absent demographic considerations?',
    'Historical and policy-document analysis of internal Israeli state deliberations on return proposals across different periods, cross-referenced with security-versus-demography framing in official statements and internal debate records where available.',
    'If demographic preservation is the dominant driver, the extraction characterization here is strongly supported. If security concerns dominate and are assessed as independently justified, part of the suppression this reading attributes to extraction might instead reflect a genuine (if contested) security-coordination function, which would not change this reading''s authored ε but would matter for a full mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_intent_ambiguity, empirical, 'Whether non-recognition of return is primarily demographic policy or primarily security policy affects how cleanly the extraction/coordination boundary can be drawn within this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement_basis(terr_tr_t1993, observed).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(terr_tr_t2000, observed).
narrative_ontology:measurement(terr_tr_t2008, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(terr_tr_t2008, observed).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2014, 0.4).
narrative_ontology:measurement_basis(terr_tr_t2014, observed).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(terr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.68).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1993, 0.62).
narrative_ontology:measurement_basis(terr_be_t1993, observed).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement_basis(terr_be_t2000, observed).
narrative_ontology:measurement(terr_be_t2008, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2008, 0.75).
narrative_ontology:measurement_basis(terr_be_t2008, observed).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2014, 0.78).
narrative_ontology:measurement_basis(terr_be_t2014, observed).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.81).
narrative_ontology:measurement_basis(terr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement_basis(terr_su_t1993, observed).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement_basis(terr_su_t2000, observed).
narrative_ontology:measurement(terr_su_t2008, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2008, 0.8).
narrative_ontology:measurement_basis(terr_su_t2008, observed).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2014, 0.83).
narrative_ontology:measurement_basis(terr_su_t2014, observed).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.86).
narrative_ontology:measurement_basis(terr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_legitimacy_dual kernel, decomposed per the ε-invariance principle because the natural-language concept 'legitimacy over this territory' covers structurally distinct claims with materially different ε values: this reading (palestinian_autochthony) authors ε=0.81 (substantially extractive, snare-classified); zionist_refuge_reading authors a low-ε legitimacy-affirming account; two_state_coexistence_reading authors a moderate-ε compromise-framework account. Each is a separate constraint story with its own stakeholders, metrics, and classification, linked here rather than merged into one observer-relative measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__palestinian_autochthony_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
