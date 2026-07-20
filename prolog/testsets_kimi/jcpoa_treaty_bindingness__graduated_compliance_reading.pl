% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Graduated Compliance Reading
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This constraint instantiates the graduated_compliance_reading of the
 *   jcpoa_treaty_bindingness kernel. It treats the JCPOA not as a binding
 *   multilateral treaty or a provisional transactional deal, but as a scaled
 *   reciprocal commitment in which enforcement (sanctions relief withdrawal)
 *   is calibrated proportionally to compliance deficits (enrichment
 *   increases), and dispute resolution prioritizes de-escalation over formal
 *   legal closure. The arrangement coordinates non-proliferation and
 *   sanctions policy among major powers while asymmetrically extracting
 *   strategic autonomy from Iran through reversible relief and snapback risk.
 *
 * KEY AGENTS:
 *   - E3/EU+3 enforcement coalition (agenda_setter / institutional / global): administers sanctions relief and snapback triggers
 *   - Iranian state (payer / institutional / global): bears compliance costs and faces reversible relief
 *   - Pragmatic diplomacy advocates (beneficiary / organized / global): gain professional and political capital from the framework
 *   - Economic actors seeking engagement (beneficiary / powerful / global): gain partial market access under snapback risk
 *   - IAEA verification body (observer / institutional / global): provides neutral technical compliance assessment
 *   - Iranian civilian population (excluded / powerless / national): bears residual sanctions costs without representation
 *   - Regional rivals demanding zero enrichment (excluded / powerful / regional): oppose the framework's tolerance for limited enrichment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.58).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.62).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '6430e03a-de01-46fc-86b0-af19dce7b753').
narrative_ontology:cs_kernel_codification('6430e03a-de01-46fc-86b0-af19dce7b753', formalized).
narrative_ontology:cs_authority_grounding('6430e03a-de01-46fc-86b0-af19dce7b753', distributed).
narrative_ontology:cs_reading_relation('6430e03a-de01-46fc-86b0-af19dce7b753', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('6430e03a-de01-46fc-86b0-af19dce7b753', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('6430e03a-de01-46fc-86b0-af19dce7b753', foundational, proportional_reciprocity_as_legitimacy).
narrative_ontology:cs_axiom_status(proportional_reciprocity_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6430e03a-de01-46fc-86b0-af19dce7b753', proportional_reciprocity_as_legitimacy, conventional).
narrative_ontology:cs_axiom('6430e03a-de01-46fc-86b0-af19dce7b753', foundational, de_escalation_over_legal_closure).
narrative_ontology:cs_axiom_status(de_escalation_over_legal_closure, holdable).
narrative_ontology:cs_axiom_grounding('6430e03a-de01-46fc-86b0-af19dce7b753', de_escalation_over_legal_closure, instrumental).
narrative_ontology:cs_reference_frame('6430e03a-de01-46fc-86b0-af19dce7b753', scaled_reciprocal_commitment_framework).
narrative_ontology:cs_drift_state('6430e03a-de01-46fc-86b0-af19dce7b753', post_2018_us_withdrawal, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6430e03a-de01-46fc-86b0-af19dce7b753', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, e3_eu_enforcement_coalition).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_engagement).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the sanctions-relief architecture, snapback triggers, and Joint Commission dispute-resolution procedures. Controls the pace of economic reintegration and the severity of reimposed measures. Benefits from verified Iranian nuclear constraints and from maintaining a coordinated great-power coalition.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, e3_eu_enforcement_coalition, agenda_setter,
    institutional, generational, constrained, global).

% Bears the costs of freezing enrichment capacity, converting facilities, and accepting intrusive IAEA inspections. Receives partial, reversible sanctions relief tied to proportional compliance. Faces graduated reimposition of sanctions for breaches. Strategic autonomy over nuclear latency is the primary extracted asset.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state, payer,
    institutional, generational, constrained, global).

% Diplomats, non-proliferation experts, and mediation NGOs whose professional legitimacy and influence depend on the survival of a multilateral diplomatic channel. They benefit from the de-escalation narrative and the institutional continuity of the Joint Commission.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% European and Asian firms and banks seeking partial trade and investment with Iran. They gain from sanctions-relief windows but remain constrained by US secondary-sanctions risk and the threat of snapback. Their engagement is tentative and reversible.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_engagement, beneficiary,
    powerful, biographical, constrained, global).

% Provides technical verification of Iranian compliance through inspections, seals, and monitoring equipment. Issues factual reports to the Joint Commission but does not set political responses or sanctions policy. Its authority is epistemic, not executive.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_body, observer,
    institutional, generational, analytical, global).

% Bears the economic brunt of residual sanctions, inflation, and currency devaluation. Not represented in compliance negotiations, dispute resolution, or snapback decisions. Would likely argue for full relief or normalized trade but has no seat at the proportional-compliance table.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_population, excluded,
    powerless, immediate, trapped, national).

% Regional states that oppose any Iranian enrichment capacity and view the graduated compliance framework as excessively lenient. They lobby for total collapse of the arrangement but are excluded from the JCPOA negotiating architecture and the Joint Commission.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_rivals_zero_enrichment, excluded,
    powerful, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, e3_eu_enforcement_coalition).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective-action problem of aligning UN, EU, and US sanctions policy behind a single, verified brake on Iranian nuclear weaponization, replacing fragmented unilateral pressure with a multilateral technical limits-and-inspections regime.
% TRANSFER_FUNCTION: Moves phased sanctions relief and international legitimacy from the E3/EU+3 coalition to Iran in exchange for verified restrictions on enrichment stockpiles and centrifuge capacity; enforcement moves economic pressure back proportionally to measured Iranian breaches.
% ABSENT_VOICES: Iranian civilians suffering under residual sanctions and regional actors demanding zero enrichment are structurally excluded from the Joint Commission. They would argue for either full relief or total collapse, positions that the proportional-compliance architecture has no procedural mechanism to accommodate.
% DISAPPEARANCE_RATIONALE: Without the graduated compliance framework, the E3/EU+3 would lack a coordinated sanctions policy, Iran would have no verified path to relief, enrichment would likely proceed unchecked or face military preemption, and the diplomatic coalition would fracture into unilateral competitive measures.
% FOUNDING_PROBLEM: The Iranian nuclear program had advanced to near-breakout capability by 2015 without a verified diplomatic brake, while unilateral US sanctions were failing to coerce total capitulation and were increasing the risk of military escalation.
% FOUNDING_PROBLEM_CORROBORATION: IAEA technical assessments and UNSC Resolution 2231 attest to the pre-2015 enrichment expansion. Independent non-proliferation analysts and former negotiators outside the direct beneficiary set corroborate the breakout-risk assessment. Critics outside the coalition, including some regional-security scholars, argue the problem was overstated to justify the sanctions-relief architecture.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate) because the constraint performs genuine coordinationâverified limits and a diplomatic channelâwhile extracting Iranian nuclear latency through a relief architecture that remains reversible. Suppression is 0.62: US secondary sanctions and the snapback mechanism actively suppress Iranian exit options and European alternative payment channels. Theater ratio is 0.40: Joint Commission meetings and diplomatic communiques perform continued cooperation, but underlying power asymmetries and US unilateral override capacity mean a substantial share of the activity maintains the appearance of proportional reciprocity rather than its full substance. Resistance is 0.55: Iranian hardliners, US unilateralists, and regional rivals actively resist the framework. Accessibility collapse is 0.45: alternatives (total war, total capitulation, unchecked proliferation) remain thinkable but are costly, so the constraint does not fully collapse the option space.
 *
 * PERSPECTIVAL GAP:
 *   The Iranian seat experiences the constraint as extractiveâthe relief is partial, reversible, and tied to intrusive inspections that erode strategic autonomyâwhile the E3/EU+3 coalition experiences it as coordination with enforcement leverage. Pragmatic diplomats and economic actors experience a rope-like benefit from the de-escalation channel, whereas Iranian civilians experience costs without representation. The engine computes this divergence from the structural data: Iranian state and civilians are declared victims with constrained exit, while the coalition and diplomatic advocates are beneficiaries with institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   The E3/EU+3 enforcement coalition and pragmatic diplomacy advocates are structural beneficiaries (low d), receiving security gains, institutional continuity, and professional legitimacy. Economic actors are secondary beneficiaries with constrained mobility. The Iranian state is the primary target (high d), paying in frozen nuclear capacity and accepted surveillance. Iranian civilians, though not direct negotiators, are structurally targeted through residual sanctions (high d). The IAEA sits near symmetric (analytical observer) because it gains no rents from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunchecked Iranian breakout riskâremains contested. The arrangement persists despite a substantial practice drift after the 2018 US withdrawal, suggesting mandatrophy pressure. However, it is not yet a piton: active enforcement (snapback threats, IAEA inspections) and genuine coordination (remaining parties still exchange limits for relief) continue to function. If the Joint Commission collapsed entirely and only theatrical statements remained, the constraint would degrade toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jcpoa_kernel_reading_decomposition,
    'Does the JCPOA text and practice support the graduated compliance reading as a coherent constraint, or does it collapse into either binding multilateral treaty status or a provisional transactional deal upon closer inspection?',
    'Comparative legal and diplomatic history analysis tracking whether the Joint Commission''s dispute-resolution practice, UNSC Resolution 2231, and state-party statements cohere around proportional reciprocity or shift toward one of the sibling readings under stress.',
    'If the text and practice do not support proportional graduated enforcement, this reading is a constructed narrative rather than a constraint inherent in the arrangement; classification could shift toward scaffold (if transitional) or snare (if the graduation story masks unilateral extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jcpoa_kernel_reading_decomposition, conceptual, 'Structural ambiguity between sibling readings of the JCPOA kernel').

omega_variable(
    unilateral_override_of_graduation,
    'Can the graduated compliance mechanism survive unilateral US withdrawal and secondary sanctions, or does the graduation collapse into coercive extraction when one party controls the global financial infrastructure?',
    'Observation of post-2018 enforcement: whether non-US parties could sustain proportional relief despite US sanctions, and whether Iran''s partial compliance was freely chosen or structurally compelled.',
    'If the US can unilaterally override the graduated scale, the constraint''s coordination function is subordinate to structural power, pushing classification toward snare for the Iranian seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_override_of_graduation, empirical, 'Whether the graduated mechanism is robust to hegemonic override').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'JCPOA treaty bindingness' conflates three structurally distinct constraints: a binding multilateral treaty reading, a graduated compliance reading, and a transactional provisional reading. Each has a distinct epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by the shared kernel but differing in authority grounding and enforcement logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
