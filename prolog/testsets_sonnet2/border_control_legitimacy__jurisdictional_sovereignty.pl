% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control as Balanced Jurisdictional Authority
 *   domain: political philosophy / international law / migration studies
 *
 * SUMMARY:
 *   This story instantiates the jurisdictional-sovereignty reading of the
 *   border-control-legitimacy kernel: sovereignty is jurisdictional authority
 *   to regulate rights and obligations within territory, but that authority
 *   does not automatically extend to unconditional border closure. Legitimacy
 *   is conditional and must be actively balanced against protection
 *   obligations (non-refoulement, asylum law) and public consent (democratic
 *   accountability for admission policy) and labor-market need. Unlike the
 *   sovereignty_primary reading, which treats exclusion as constitutive of
 *   statehood, or the freedom_of_movement_primary reading, which treats
 *   movement as a trumping right, this reading treats admission decisions as
 *   the output of an ongoing, contestable balancing test — which means the
 *   framework can fail toward either excess exclusion (protection violated)
 *   or excess admission relative to processed public consent (consent
 *   violated), and both failure modes have named victims in this story.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: Primary agenda-setter (institutional/analytical) — administers the balancing test and controls enforcement
 *   - receiving_state_publics: Primary domestic beneficiary and secondary payer (organized/constrained) — consent-based legitimacy depends on perceived calibration
 *   - citizen_labor_market_incumbents: Beneficiary (moderate/constrained) — served by labor-calibrated admission
 *   - excluded_asylum_seekers: Primary target of exclusion-side failure (powerless/trapped) — bear cost when protection balancing tilts restrictive
 *   - undocumented_labor_migrants: Primary target of labor-side failure (powerless/trapped) — bear cost when legal channels under-match actual demand
 *   - displaced_citizens_denied_family_reunification: Citizen-side victim (powerless/constrained) — shows the framework produces victims even among full jurisdictional members
 *   - international_human_rights_bodies: Analytical observer (institutional/analytical) — monitors whether balancing is substantive or rhetorical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.52).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.58).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control as Balanced Jurisdictional Authority").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political philosophy / international law / migration studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '48b089b1-c1a8-4dbe-b17e-aece438209a9').
narrative_ontology:cs_kernel_codification('48b089b1-c1a8-4dbe-b17e-aece438209a9', distributed).
narrative_ontology:cs_authority_grounding('48b089b1-c1a8-4dbe-b17e-aece438209a9', distributed).
narrative_ontology:cs_reading_relation('48b089b1-c1a8-4dbe-b17e-aece438209a9', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('48b089b1-c1a8-4dbe-b17e-aece438209a9', border_control_legitimacy__freedom_of_movement_primary, influences).
narrative_ontology:cs_axiom('48b089b1-c1a8-4dbe-b17e-aece438209a9', foundational, sovereignty_conditional_on_balancing).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_balancing, holdable).
narrative_ontology:cs_axiom_grounding('48b089b1-c1a8-4dbe-b17e-aece438209a9', sovereignty_conditional_on_balancing, conventional).
narrative_ontology:cs_axiom('48b089b1-c1a8-4dbe-b17e-aece438209a9', secondary, protection_obligation_constrains_but_does_not_abolish_discretion).
narrative_ontology:cs_axiom_status(protection_obligation_constrains_but_does_not_abolish_discretion, holdable).
narrative_ontology:cs_axiom_grounding('48b089b1-c1a8-4dbe-b17e-aece438209a9', protection_obligation_constrains_but_does_not_abolish_discretion, deontological).
narrative_ontology:cs_reference_frame('48b089b1-c1a8-4dbe-b17e-aece438209a9', post_war_qualified_sovereignty_settlement).
narrative_ontology:cs_drift_state('48b089b1-c1a8-4dbe-b17e-aece438209a9', contemporary_securitized_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48b089b1-c1a8-4dbe-b17e-aece438209a9', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, citizen_labor_market_incumbents).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_publics).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_administrative_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, undocumented_labor_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_denied_family_reunification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_publics).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, proportionality_constrained_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, balancing_test_legitimacy_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers entry, asylum processing, and removal within a jurisdiction it claims exclusive regulatory authority over. Justifies border enforcement as one lever among several (labor policy, family reunification, asylum obligations) rather than an unconditional gate, and is formally bound by proportionality and necessity review, treaty obligations, and domestic courts — though it sets the operational rules and controls the enforcement budget.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Consent to the political legitimacy of the state depends partly on perceived control over composition and pace of admission. Benefit from orderly labor supply and public-service planning that regulated admission enables, but also bear costs when enforcement is seen as either too lax (perceived loss of consent-based control) or too harsh (moral and reputational costs, family separations in mixed-status communities).
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_publics, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_publics, payer).

% Benefit from admission policy calibrated to labor needs — enough migration to fill genuine shortages without wage suppression in adjacent sectors. Their interest is served by the balancing framework rather than either open borders or closure, and they have organized political voice (unions, trade associations) to press for calibration.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, citizen_labor_market_incumbents, beneficiary,
    moderate, biographical, constrained, national).

% Seek protection from persecution or violence and are subject to admission decisions made under proportionality tests that, in this reading, are supposed to weigh protection obligations against public consent and administrative capacity — but in practice can result in refusal, detention, or return to danger when the balancing is administered restrictively. Their exit options are essentially nonexistent once denied; the same jurisdictional authority that admits also excludes.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Work inside the jurisdiction without full legal status, filling labor demand that the balancing framework is supposed to address through legal channels but often does not (visa caps, processing delays, employer sponsorship barriers). Bear enforcement risk (detention, deportation, wage theft with no legal recourse) precisely because the coordination function the framework claims to provide has not been extended to them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, undocumented_labor_migrants, payer,
    powerless, biographical, trapped, national).

% Citizens or lawful residents whose spouses, children, or parents are barred or delayed under the same balancing framework, on the theory that admission volume must be weighed against public consent and capacity. They bear the human cost of the state's own citizens' familial rights being subordinated to aggregate admission calibration, despite holding full jurisdictional membership themselves.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_denied_family_reunification, payer,
    powerless, biographical, constrained, national).

% Monitor whether the state's balancing test genuinely constrains enforcement (proportionality, necessity, non-refoulement) or merely provides rhetorical cover for outcomes indistinguishable from unconstrained exclusion. Issue findings, but enforcement of those findings depends on the state's own cooperation.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__jurisdictional_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(border_control_legitimacy__jurisdictional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework in which a state can regulate who may work, reside, and claim protection within its territory in a way that is answerable to multiple legitimate claims at once — international protection obligations, domestic labor-market needs, and the political consent of the resident public — rather than treating any one of these as automatically dispositive.
% TRANSFER_FUNCTION: Moves the burden of uncertainty and risk from the administering state (which retains discretion and control over pace/composition of admission) onto individual migrants and their citizen family members, who absorb the costs of denial, delay, and enforcement even when the underlying balancing test is met on paper but administered restrictively in practice.
% ABSENT_VOICES: Excluded asylum seekers and undocumented migrants are not parties to the domestic political processes (elections, legislative hearings) that set the balancing test's actual thresholds; their interests are represented, if at all, through advocacy organizations and international bodies rather than direct voice. Displaced citizen family members have formal voice as citizens but are frequently outvoted by aggregate public-consent considerations.
% DISAPPEARANCE_RATIONALE: If the jurisdictional-sovereignty balancing framework disappeared overnight, states would either revert to unconstrained exclusion (sovereignty_primary) or open admission bound only by rights claims (freedom_of_movement_primary) — either shift would immediately restructure labor markets, asylum processing, family reunification law, and the political basis on which publics currently consent to the administrative state's border authority. The current arrangement is a load-bearing structure, not an inert description of natural fact.
% FOUNDING_PROBLEM: Twentieth-century international law needed a way to reconcile the Westphalian premise of exclusive territorial jurisdiction with post-WWII protection obligations (refugee conventions, human rights instruments) and the practical reality that closed borders were neither economically nor administratively tenable — hence a doctrine of qualified, proportionality-bound sovereign discretion rather than either absolute closure or open movement.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and human rights treaty bodies attest the protection-balancing problem remains live and is frequently under-enforced by administering states. Labor economists and business associations attest the labor-needs balancing problem remains live and is poorly calibrated (visa backlogs exceeding actual quota utility). Restrictionist political movements, external to any beneficiary group named here, dispute that public-consent balancing is being honored at all, arguing enforcement has drifted toward performative rather than substantive control — a critique this reading treats as evidence of contested rather than resolved status, not as evidence against the framework's founding rationale.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52 at interval end) is moderate rather than high because the reading's own premise is that the arrangement performs a real coordination function — reconciling protection, labor, and consent claims that genuinely conflict — not pure rent extraction. But it is well above negligible because the balancing test, once institutionalized, structurally advantages the administering state's discretion over the individuals whose claims are being 'balanced,' and the state controls both the criteria and the enforcement budget. Suppression (0.58) reflects that exclusion decisions are backed by detention and removal machinery, which is real coercive force even when procedurally constrained. Theater ratio (0.34, rising over the interval) reflects an accumulating gap between the proportionality/necessity rhetoric used to justify enforcement expansion and the degree to which that rhetoric constrains actual administrative outcomes — a drift worth tracking but not yet dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the state administrative apparatus's seat, the arrangement looks like ongoing good-faith balancing constrained by law and courts. From the excluded asylum seeker's or undocumented migrant's seat, the same apparatus computes as an extraction and exclusion mechanism whose 'balancing' language does not change that a single denial is total and often irreversible. The engine computing different per-seat types from the same structural data is the point — this reading's whole distinguishing claim is that legitimacy is conditional and contestable, which means different seats are entitled to compute genuinely different verdicts from the same facts.
 *
 * DIRECTIONALITY LOGIC:
 *   The state administrative apparatus sits near the agenda-setting end: it writes and applies the balancing criteria and is only weakly constrained by external review. Receiving-state publics and labor-market incumbents are beneficiaries whose consent and labor needs the framework is explicitly built to serve, though publics also bear diffuse costs when the balance drifts either direction. Excluded asylum seekers, undocumented labor migrants, and displaced citizen family members are the three victim classes this reading distinctively acknowledges (unlike either sovereignty_primary, which has only excluded migrants as visible costs it treats as legitimate, or freedom_of_movement_primary, which treats any exclusion as illegitimate by definition) — their trapped or constrained exit options and powerless standing place them at the high end of derived directionality regardless of which failure mode (over-exclusion or under-admission) produced their harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabeling errors at once: it resists calling the arrangement a pure Rope (which would erase the three victim classes and treat the balancing test as costless coordination), and it resists calling it a pure Snare (which would erase the genuine coordination function the balancing test performs when protection, labor, and consent claims are jointly and honestly weighed). Tangled Rope is the structurally honest classification because the SAME apparatus that coordinates (processes claims, allocates visas, adjudicates asylum) is the one that extracts (detains, denies, deports) — coordination and extraction run through one enforcement structure, which is exactly the Tangled Rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_substantive_or_rhetorical,
    'Does the proportionality/necessity balancing test genuinely constrain state discretion in individual admission and removal decisions, or does it function primarily as ex-post legal justification for decisions made on other grounds (political pressure, administrative capacity, enforcement quotas)?',
    'Comparative review of case-level outcomes against stated balancing criteria across jurisdictions claiming this framework; tracking whether judicial or human-rights-body findings of balancing-test violations produce actual policy change or are absorbed without consequence.',
    'If substantive, this reading''s Tangled Rope classification is well-supported — a real coordination function operates alongside real extraction. If rhetorical, the constraint collapses toward the sovereignty_primary reading''s structure in practice even while retaining this reading''s legal language, which would be a significant finding about doctrine-practice drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_substantive_or_rhetorical, empirical, 'Whether the balancing test constrains outcomes or merely narrates them.').

omega_variable(
    public_consent_measurement_problem,
    'How is ''public consent'' to admission policy actually measured or operationalized by the state, and does the measurement mechanism itself introduce bias (e.g., overweighting vocal restrictionist minorities, underweighting affected migrant communities who cannot vote)?',
    'Analysis of which political and institutional mechanisms translate public sentiment into admission-policy inputs (elections, polling, legislative committees, executive discretion) and whose voice those mechanisms actually register.',
    'If consent-measurement systematically underweights affected non-citizen populations while overweighting incumbent-citizen preferences, the ''balancing'' is structurally asymmetric even when procedurally fair, which would push the effective classification further toward extraction than the doctrine acknowledges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_consent_measurement_problem, conceptual, 'Whether the consent input to the balancing test is itself fairly constructed.').

omega_variable(
    kernel_framing_under_determination,
    'Is ''jurisdictional sovereignty'' a genuinely distinct, stable middle position between sovereignty_primary and freedom_of_movement_primary, or is it better understood as an unstable compromise formula that different administrations fill in with either extreme depending on political conditions?',
    'Longitudinal tracking of a single jurisdiction''s stated adherence to this balancing doctrine across administrations with different political orientations, checking whether outcomes cluster near one pole despite consistent doctrinal language.',
    'If the doctrine is unstable in practice, this reading may be better modeled as a rhetorical overlay on whichever of the two polar readings is politically ascendant at a given time, rather than as an independent structural claim with its own stable epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the balancing-doctrine reading is a stable third position or an unstable label applied to shifting polar outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 8, 0.22).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 16, 0.26).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 24, 0.29).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 32, 0.32).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 40, 0.34).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language concept 'border control legitimacy,' per the eps-invariance principle. sovereignty_primary authors near-total discretion and treats exclusion as constitutive of statehood (low acknowledged extraction from its own vantage, since exclusion is definitionally legitimate). freedom_of_movement_primary authors border closure itself as the extractive act (high epsilon, movement as trumping right). This reading (jurisdictional_sovereignty) sits structurally between them: it acknowledges real coordination function AND real extraction running through the same enforcement apparatus, with dual victim sets (excluded migrants and displaced citizens) that neither polar reading names together. Each story carries its own stable epsilon, its own beneficiary/victim structure, and its own claimed type; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
