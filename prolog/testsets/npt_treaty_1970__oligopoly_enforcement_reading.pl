% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Oligopoly Enforcement (Articles I-II Binding, Article VI Aspirational)
 *   domain: international_law/security
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (entered into force 1970) is
 *   presented by its architects and the Nuclear Weapons States as a binding
 *   mutual obligation: NNWS renounce nuclear weapons development in exchange
 *   for NWS disarmament (Article VI) and access to peaceful nuclear
 *   technology (Article IV). This is the oligopoly enforcement reading, which
 *   instantiates a different structural claim: Articles I-II (prohibiting
 *   transfer to and receipt by NNWS) are binding and actively enforced,
 *   creating a two-tier regime where horizontal proliferation is policed
 *   while vertical proliferation (NWS arsenal development) remains unpoliced.
 *   Article VI functions as aspirational political language, indefinitely
 *   deferred and structurally unenforceable because the NWS control the
 *   Security Council and veto any enforcement mechanism. The founding bargain
 *   has atrophied into a liability imposed on threshold and developing states
 *   without reciprocal NWS obligation. This reading competes with two
 *   siblings: the reciprocal_disarmament_reading (Article VI as binding legal
 *   obligation with temporal urgency) and the withdrawal_sovereignty_reading
 *   (Article X as legitimizing exit from an inequitable bargain). These are
 *   not disagreements about facts; they are disagreements about which
 *   commitments the treaty actually embodies and which should be honored.
 *
 * KEY AGENTS:
 *   - nws_oligarchy: Institutional power, sets agenda, defines compliance, enforces horizontally while exempting vertically.
 *   - threshold_states: Powerful organizationally, high regional relevance, but trapped by inspection burden and denied the deterrent capability the NWS retain.
 *   - non_aligned_nnws: Moderate power, comply without security gain, represent plurality of states but lack enforcement voice.
 *   - developing_nnws: Powerless structurally, trapped by export controls and inspections despite zero proliferation capacity.
 *   - iaea_inspectorate: Institutionally constrained, audits only NNWS, bears operational burden without mandate authority.
 *   - treaty_review_conference: Organized observership, consensus-ruled into paralysis, functions theatrically.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.71).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Oligopoly Enforcement (Articles I-II Binding, Article VI Aspirational)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/security").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, '595ccbff-113f-41a1-97ce-81e1b50f214d').
narrative_ontology:cs_kernel_codification('595ccbff-113f-41a1-97ce-81e1b50f214d', fixed_text).
narrative_ontology:cs_authority_grounding('595ccbff-113f-41a1-97ce-81e1b50f214d', extraction).
narrative_ontology:cs_interpretation_layer_present('595ccbff-113f-41a1-97ce-81e1b50f214d').
narrative_ontology:cs_reading_relation('595ccbff-113f-41a1-97ce-81e1b50f214d', npt_treaty_1970__reciprocal_disarmament_reading, forecloses).
narrative_ontology:cs_reading_relation('595ccbff-113f-41a1-97ce-81e1b50f214d', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('595ccbff-113f-41a1-97ce-81e1b50f214d', foundational, horizontal_proliferation_prohibition_primary_binding).
narrative_ontology:cs_axiom_status(horizontal_proliferation_prohibition_primary_binding, holdable).
narrative_ontology:cs_axiom_grounding('595ccbff-113f-41a1-97ce-81e1b50f214d', horizontal_proliferation_prohibition_primary_binding, conventional).
narrative_ontology:cs_axiom('595ccbff-113f-41a1-97ce-81e1b50f214d', foundational, vertical_disarmament_obligation_indefinitely_deferrable).
narrative_ontology:cs_axiom_status(vertical_disarmament_obligation_indefinitely_deferrable, holdable).
narrative_ontology:cs_axiom_grounding('595ccbff-113f-41a1-97ce-81e1b50f214d', vertical_disarmament_obligation_indefinitely_deferrable, instrumental).
narrative_ontology:cs_reference_frame('595ccbff-113f-41a1-97ce-81e1b50f214d', two_tier_enforcement_regime).
narrative_ontology:cs_drift_state('595ccbff-113f-41a1-97ce-81e1b50f214d', contemporary_post_2015, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('595ccbff-113f-41a1-97ce-81e1b50f214d', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nws_oligarchy).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nws_permanent_security_council).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_aligned_nnws).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, developing_nnws).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 and rising (from 0.45 in 1970) because the hidden transfer—sovereignty to inspect NNWS, technology denial, cost of compliance verification—flows outward while the reciprocal obligation (NWS disarmament) atrophies into unenforceable language. Suppression is 0.71 and stable at high level because the constraint requires active enforcement: NNWS inspection regimes, export control intelligence, diplomatic pressure on threshold states, and P5 veto on any enforcement mechanism that would apply to themselves. Theater is 0.52 and rising (from 0.15 in 1970) because Review Conferences theatrically demand Article VI compliance while NWS present 'disarmament commitments' that reset indefinitely without measurable reduction. The rising theater_ratio tracks the divergence between the founding bargain framing and the actual operation—inspections are real, disarmament is performative. Accessibility_collapse shows a four-level gradient: structural (0.76, highly constrained) >> organizational (0.71, constrained) > class (0.63, moderately constrained) > individual (0.52, some latitude within state compliance). Suppression similarly shows institutional gradient: structural suppression (0.81) is the veto-rule enforcement, organizational (0.74) is IAEA enforcement, class-level (0.68) is coalition pressure, individual (0.58) is domestic political room for maneuver within state leadership. Resistance (0.71 structural level, 0.52 individual) is mounted strongest at individual/civil-society level (abolition movements) and weakest at the structural level where the P5 consensus prevents any binding change.
 *
 * PERSPECTIVAL GAP:
 *   The NWS agenda-setter seat and the threshold-state payer seats compute radically differently. From the NWS institutional seat: the NPT is a successful coordination mechanism that has prevented 15-20 additional weapons states and legitimized their deterrence posture. From the threshold-state organizational seat: the NPT is an asymmetric constraint that denies them the deterrent the NWS claim is necessary while refusing disarmament. The engine computes this as two per-seat classifications: the NWS seat may compute as rope (coordination with modest asymmetry) while the threshold-state seat computes as snare (pure extraction dressed as coordination). The gap is not a defect—it is the signal the oligopoly reading instantiates. The divergence IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are near d=0.0 (full beneficiary): they set the rules, enforce horizontally while exempting themselves vertically, benefit from the legitimacy of non-proliferation framing while bearing none of the costs of disarmament. Threshold states are near d=0.95 (near-full target): they bear inspection burden, technology denial, strategic vulnerability (denied deterrent while rivals acquire weapons), and cannot exit without severe costs. Non-aligned NNWS sit near d=0.80 (high target): they comply, incur verification costs, receive no security benefit, but their compliance threat is lower than threshold states. Developing NNWS are near d=0.88 (very high target): maximum compliance cost (inspections they cannot evade, export controls), zero proliferation capacity, cannot contribute to any resistance coalition. The IAEA sits near d=0.50 (symmetric): it coordinates inspection but is suppressed by lack of NWS access and constrained funding from the same states it cannot inspect. No overrides needed—the structural derivation from beneficiary/victim declarations and exit_options produces these values accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The oligopoly enforcement reading routes through a mandatrophy lens: the founding mandate ('mutual renunciation of weapons in exchange for disarmament by NWS') is dead—no NWS has disarmed in 56 years, and threshold states now view Article VI as a cover story. The constraint persists not because the mandate is live but because the extraction mechanism (inspections, export controls, Security Council veto on self-application) serves the beneficiaries (NWS oligarchy). A genuine Tangled Rope would show both coordination and asymmetric extraction as inseparable—the inspection regime genuinely contains horizontal proliferation AND extracts sovereignty costs from NNWS. But the rising theater_ratio (0.52) suggests the coordination function is atrophying relative to the extraction function. The mandatrophy verdict: this constraint has outlived its mandate. It persists as inertia + extraction, not as coordination. This is the piton classification risk (mostly performance, no living function) except for one detail: Articles I-II enforcement IS still functional and real—it does prevent horizontal proliferation (Indian, Pakistani, Israeli cases notwithstanding show the regime bends but holds). The classification is not piton but tangled_rope in mandatrophy—it is a genuine hybrid that has shifted from coordination-heavy to extraction-heavy as the reciprocal obligation (NWS disarmament) died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status_ambiguity,
    'Is Article VI a binding legal obligation with temporal force, or a political aspiration indefinitely deferrable by the NWS?',
    'International Court of Justice advisory opinion on Article VI''s enforceability; or a binding NWS commitment to nuclear reduction with verification timeline (would resolve by action, not adjudication).',
    'If binding: the NPT is a violated treaty and threshold states have justification to exit or demand remedies. If aspirational: the treaty is working as the oligopoly reading claims—horizontal proliferation contained, vertical proliferation unrestricted. This is THE structural question that distinguishes the oligopoly_enforcement_reading from reciprocal_disarmament_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_status_ambiguity, conceptual, 'Whether Article VI imposes binding legal obligation on NWS or functions as aspirational political language.').

omega_variable(
    horizontal_proliferation_containment_efficacy,
    'Are Articles I-II inspection and enforcement regimes actually preventing horizontal proliferation, or merely obscuring threshold-state weapons programs?',
    'Full forensic investigation of threshold-state programs (India, Pakistan, Israel, Iran) with access to classified intelligence; comparative analysis of proliferation trajectory with/without NPT regime (counterfactual modeling).',
    'If Articles I-II are efficacious: the constraint''s coordination function is real and the extraction (NNWS compliance costs) is justified by security benefit. If regimes are obscured but not prevented: the constraint is extractive theater—NNWS pay compliance costs while threshold states acquire weapons anyway, and the NWS gain legitimacy without results. The high accessibility_collapse (0.76 structural level) assumes efficacy; efficacy failure would lower collapse and suggest the constraint is less structural than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_proliferation_containment_efficacy, empirical, 'Whether Articles I-II regimes prevent proliferation or merely document it asymmetrically.').

omega_variable(
    threshold_state_deterrent_rationale,
    'Do threshold states pursue weapons programs because the NPT denies them deterrent explicitly, or because regional security dilemmas exist independent of the treaty?',
    'Counterfactual modeling: if the NPT had Article VI enforcement (NWS disarmed), would threshold-state weapons programs still emerge? Evidence from threshold-state strategic documents and threat perception analysis.',
    'If NPT-denial is causal: the treaty actively creates the security dilemmas it claims to prevent (forced to choose between renounced deterrent and existential vulnerability). If dilemmas pre-exist: threshold-state weapons programs are security-rational independent of treaty structure, and the extraction borne by NNWS is merely the price of their security choice. This resolves the directionality logic: if NPT-denial is causal, threshold states are victims (forced into vulnerable position); if security dilemmas pre-exist, threshold states are rational actors, not victims, and the classification shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_deterrent_rationale, empirical, 'Whether NPT restriction of deterrent capability creates or merely responds to regional security dilemmas.').

omega_variable(
    sibling_reading_foreclosure_structural_status,
    'Can the oligopoly_enforcement_reading and the reciprocal_disarmament_reading coexist in a single institutional framework, or does accepting one logically rule out the other?',
    'Textual analysis of the treaty language and legal interpretation: if the NWS explicitly hold the oligopoly interpretation (Articles I-II binding, Article VI aspirational) while NNWS hold the reciprocal interpretation (Articles inseparable, VI binding), do they occupy the same legal framework or different ones? Institutional question: can one framework embody both readings simultaneously?',
    'If they coexist: the readings compete across parties but neither forecloses the other—a ''coexists_with'' relation holds. If they foreclose: one reading''s core premise logically rules out the other''s within any single legal system, and the treaty itself embodies an unresolved contradiction—a ''forecloses'' relation holds. This determines the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_structural_status, conceptual, 'Whether the oligopoly and reciprocal readings are logically incompatible or merely represent different parties'' interpretations of an ambiguous text.').

omega_variable(
    nws_security_council_veto_legitimacy,
    'Does the P5''s use of Security Council veto to prevent binding enforcement of Article VI on themselves represent a legitimate safeguard of sovereignty, or a structural corruption of the treaty''s purpose?',
    'No empirical resolution; this is a preference question. Preference stakes: if veto is legitimate, the oligopoly reading reflects proper institutional structure; if veto is corrupt, the oligopoly reading describes a broken system that should be reformed.',
    'The interpretation shapes the classification''s normative standing but not its structural truth. Structurally, the veto IS the mechanism that makes extraction of Articles I-II from NNWS while exempting NWS from Article VI possible. Whether that mechanism is justified is a separate preference question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nws_security_council_veto_legitimacy, preference, 'Whether the P5''s Security Council veto on self-application of Article VI is a legitimate sovereignty safeguard or an institutional corruption of treaty purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2020, 0.49).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2026, 0.52).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1985, 0.63).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2000, 0.67).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(npt__su_t2026, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_deterrence_asymmetry).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, security_council_veto_power).

% DUAL FORMULATION NOTE:
% The npt_treaty_1970 kernel decomposes into three structurally distinct constraints corresponding to three readings of which articles are binding and which are aspirational. The oligopoly_enforcement_reading treats Articles I-II as binding and Article VI as aspirational, creating a two-tier regime. The reciprocal_disarmament_reading treats all three articles as inseparable and binding, creating a symmetrical obligation regime. The withdrawal_sovereignty_reading treats Article X as legitimizing exit. These are NOT alternate measurements of one constraint—they are different constraints instantiated by different interpretations of the same text. Each has its own epsilon, its own stakeholder asymmetries, and its own classification. The oligopoly reading's core claim is that the treaty's real structure is Articles I-II binding (and enforced) + Article VI aspirational (and indefinitely deferred), which makes the NWS beneficiaries of a two-tier regime that extracts sovereignty costs from NNWS without reciprocal disarmament. The reciprocal reading's core claim is that this interpretation betrays the founding bargain and violates Article VI's binding language. These are structurally distinct constraints linked by network causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
