% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Norm Complex: Liberal Institutional Reading
 *   domain: international_relations/international_law
 *
 * SUMMARY:
 *   The RBIO (Responsibility to Protect, International Humanitarian Order)
 *   norm complex is a reading of contested multilateral governance
 *   principles. This story instantiates the LIBERAL INSTITUTIONAL READING:
 *   RBIO norms are claimed as universal, consent-based, and revisable through
 *   legitimate multilateral process. Enforcement selectivity (the fact that
 *   some states' violations are overlooked while others face intervention and
 *   sanctions) is interpreted as a capacity problem — resource constraints,
 *   great-power disagreement, and practical coordination difficulty — not as
 *   evidence that the system is fundamentally extractive or hegemonic. The
 *   sibling readings (hegemonic extraction and sovereignty maximalism)
 *   diagnose the same selectivity as intentional and structural. The temporal
 *   data show extractiveness rising from 1945 to 2005, then plateauing;
 *   theater ratio rising monotonically from 1945 to 2005, then stabilizing;
 *   and suppression requirement rising through 2005 then plateauing. This
 *   pattern is consistent with a system that consolidated (1945–2005) and
 *   then stabilized (2005–2025), with an increasing share of enforcement
 *   activity devoted to maintaining P5 authority over the multilateral
 *   apparatus rather than to coordination function.
 *
 * KEY AGENTS:
 *   - P5 states (USA, Russia, China, UK, France): institutional power, set the UNSC agenda, veto amendments, define enforcement scope
 *   - Targeted sovereigns (states subject to intervention or sanctions): trapped, excluded from amendment process, bear extraction costs
 *   - Civilian populations under sanctions: powerless, face direct humanitarian costs, no voice in decision
 *   - Enforcement contractors (military, peacekeeping, monitoring firms): organized, benefit from legitimacy frame, mobile exit
 *   - Multilateral institutional apparatus (UNSC, General Assembly, courts): set procedures, derive significance from role as norm arbiter
 *   - Sovereignty-advocate states: excluded, powerful enough to resist but not to reshape the system, constrained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.62).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.71).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Norm Complex: Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '1ac34a72-0965-48a9-bb99-386b04d8536b').
narrative_ontology:cs_kernel_codification('1ac34a72-0965-48a9-bb99-386b04d8536b', fixed_text).
narrative_ontology:cs_authority_grounding('1ac34a72-0965-48a9-bb99-386b04d8536b', extraction).
narrative_ontology:cs_interpretation_layer_present('1ac34a72-0965-48a9-bb99-386b04d8536b').
narrative_ontology:cs_reading_relation('1ac34a72-0965-48a9-bb99-386b04d8536b', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ac34a72-0965-48a9-bb99-386b04d8536b', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('1ac34a72-0965-48a9-bb99-386b04d8536b', foundational, multilateral_process_legitimates_enforcement).
narrative_ontology:cs_axiom_status(multilateral_process_legitimates_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('1ac34a72-0965-48a9-bb99-386b04d8536b', multilateral_process_legitimates_enforcement, conventional).
narrative_ontology:cs_axiom('1ac34a72-0965-48a9-bb99-386b04d8536b', foundational, selectivity_is_capacity_not_design).
narrative_ontology:cs_axiom_status(selectivity_is_capacity_not_design, holdable).
narrative_ontology:cs_axiom_grounding('1ac34a72-0965-48a9-bb99-386b04d8536b', selectivity_is_capacity_not_design, empirically_contingent).
narrative_ontology:cs_axiom('1ac34a72-0965-48a9-bb99-386b04d8536b', secondary, consent_compatible_with_veto_structure).
narrative_ontology:cs_axiom_status(consent_compatible_with_veto_structure, holdable).
narrative_ontology:cs_axiom_grounding('1ac34a72-0965-48a9-bb99-386b04d8536b', consent_compatible_with_veto_structure, deontological).
narrative_ontology:cs_reference_frame('1ac34a72-0965-48a9-bb99-386b04d8536b', universal_multilateral_consent_basis).
narrative_ontology:cs_drift_state('1ac34a72-0965-48a9-bb99-386b04d8536b', contemporary_multipolarity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1ac34a72-0965-48a9-bb99-386b04d8536b', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_p5_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_enforcement_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutional_apparatus).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_sovereigns).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, human_rights_advocacy_coalition).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, non_p5_sovereigns_and_developing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the agenda for UNSC authorization of military and economic enforcement action. Collect legitimacy for intervention, access to enforcement contracts, and geopolitical positioning as norm protectors. Frame interventions as multilateral institutional decisions grounded in universal consent-based process. Their veto power over amendments means they can block codification challenges while claiming the system is revisable.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_p5_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, intervening_p5_states, beneficiary).

% Receive contracts for military operations, peacekeeping, reconstruction, sanctions monitoring, and humanitarian intervention under RBIO mandates. Benefit from the legitimacy framework: operations framed as norm enforcement rather than conquest attract funding, personnel, and political cover. Exit is available — they can pursue other contracts — but RBIO enforcement work is a reliable revenue stream and status source.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_enforcement_contractors, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of military intervention, sanctions regimes, and conditional restructuring framed as norm compliance. Are excluded from the core UNSC decision-making process (unless P5 members themselves). Argument that the system is consent-based and revisable is cold comfort when P5 vetoes prevent amendment and enforcement selectivity means their violations are overlooked while adversaries face intervention. Cannot exit the international system; can only absorb or resist.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_sovereigns, payer,
    moderate, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, targeted_sovereigns, excluded).

% Experience the direct human cost of sanctions regimes imposed under RBIO authority: food insecurity, medical supply shortages, economic collapse, diaspora. Frame justifies these costs as necessary to enforce universal norms; the reading asserts this is enforcement-capacity limitation, not design. The powerless have no seat at the decision table and cannot exit the jurisdiction targeted by sanctions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions, payer,
    powerless, biographical, trapped, global).

% Reject the liberal institutional reading as a cover for hierarchical intervention. Argue the norm complex is a tool of P5 hegemony. Are excluded from the revisions process because the P5 veto prevents amendment and because challenging the reading itself is read as non-cooperation with universal norms. Would argue for absolute sovereignty and regional autonomy but that position is delegitimized within the liberal framework.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_advocate_states, excluded,
    powerful, generational, constrained, global).

% UNSC, General Assembly, regional bodies, and norm-adjudicating institutions collectively benefit from the reading: it sustains their authority as the legitimate site for norm interpretation and enforcement. Enforcing the norm complex through their procedures amplifies their institutional significance and resource flow. The reading treats them as neutral arbiters; the system benefits from remaining structured as an arena where consent can be expressed and revisions negotiated, even when the P5 veto makes substantial amendment impractical.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutional_apparatus, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutional_apparatus, agenda_setter).

% Invokes the liberal reading to justify interventions against grave atrocities and to call for enforcement consistency. Benefits from the norm complex by gaining legitimacy for advocacy: the reading provides a principled framework for conditional support of intervention and sanctions. Can exit if the reading's promise of universality and consent-basis fails; currently committed because the reading offers leverage for their cause.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, human_rights_advocacy_coalition, beneficiary,
    organized, biographical, mobile, global).

% Bear costs of enforcement selectivity (their violations or those of powerful allies are overlooked) and of conditional aid regimes that embed economic restructuring as norm compliance. Excluded from revising the system because the P5 veto structure and great-power consensus makes amendment practically impossible despite the reading's assertion that the system is revisable. Can vote in General Assembly but that body has no enforcement power. Are constrained to work within the system they cannot reshape.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, non_p5_sovereigns_and_developing_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, non_p5_sovereigns_and_developing_states, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, intervening_p5_states).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common framework for legitimate humanitarian intervention, sanctions, and norm enforcement that transcends unilateral state action. Solves the problem of coordination among sovereigns on shared concerns (atrocities, sovereignty violations, weapons proliferation) by channeling enforcement through multilateral institutional processes — UNSC authorization, treaty ratification, General Assembly legitimation. The reading asserts this prevents a war-of-all-against-all in norm enforcement and provides transparency and consent mechanisms.
% TRANSFER_FUNCTION: Transfers authority, resources, and legitimacy from targeted sovereigns and their civilian populations to intervening P5 states, enforcement contractors, and the multilateral institutional apparatus. Specifically: moves military and reconstruction contracts to contractors; moves geopolitical positioning and norm-setting authority to P5 states; moves institutional significance and funding to multilateral bodies; extracts compliance costs and sanctions burden from targets and their populations.
% ABSENT_VOICES: Sovereignty-maximalist states (especially rising powers and those with past intervention experience) are structurally excluded from core decision-making and from amending the RBIO framework because the P5 veto structure prevents it. Their objection — that the norm complex privileges P5 interests under a universalist mask — is not seated at the table. Targeted sovereigns' civilian populations lack any voice in decisions that impose sanctions costs on them. Alternative frameworks (regional hegemony, multipolar deference zones) are delegitimized within the liberal reading and cannot be negotiated because the reading frames them as illegitimate from the start.
% DISAPPEARANCE_RATIONALE: If the RBIO norm complex and its enforcement apparatus disappeared, international order would reorganize: regional powers would reassert sphere-of-influence claims; humanitarian enforcement would fragment into unilateral action; sanctions regimes would lose institutional legitimacy; the P5 would still possess military capability but lose the institutional leverage that allows them to act as norm arbiters rather than naked power. The system's institutions (UNSC, treaty bodies, humanitarian courts) would lose their primary enforcement mandate.
% FOUNDING_PROBLEM: Post-WWII international governance needed a framework to prevent unilateral military conquest and to coordinate responses to atrocities and sovereignty violations without dissolving into great-power war. The RBIO norm complex was designed to provide universal principles (state sovereignty, human rights, non-interference except under multilateral authorization) and decision-making procedures (UNSC, treaty ratification, General Assembly) that both constrain unilateralism and provide legitimate channels for enforcement.
% FOUNDING_PROBLEM_CORROBORATION: The liberal institutional reading attests the founding problem remains live: unilateral military action still occurs, atrocities still demand response, and coordination is essential. Sovereignty-maximalist states and targeted sovereigns attest the founding problem has been solved structurally (great-power deterrence + sovereignty principle) and the norm complex now serves as a tool of P5 hegemony rather than prevention of conquest. International legal scholarship is split: some scholars (liberal institutionalists, human rights advocates) support the reading; others (critical scholars, sovereignty advocates, post-colonial analysts) contend the founding function is pretense and the extraction function is primary.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the liberal reading asserts universality and consent-basis while the P5 veto structure makes amendments practically impossible, and because enforcement is selective in ways that benefit P5 states and their allies. Suppression (0.71) is high because maintaining the reading against the hegemonic extraction and sovereignty-maximalist challenges requires sustained institutional work: delegitimizing alternative readings, enforcing compliance narratives, preventing codification of amendments that would constrain P5 power. Theater (0.48) is moderate-high: the coordination function is real (great powers do coordinate on some norms), but an increasing share of institutional activity is devoted to legitimating why enforcement is selective and to defending the reading itself against challengers. The measurement series show extraction and suppression rising from 1945 to 2005 as the system consolidated and the stakes of maintaining it increased (Cold War deterrence, post-Cold War unipolarity, then the challenge of multipolarity from 2005 onward). Theater rises throughout because the gap between the reading's claim to universality and the observed selectivity requires continuous narrative work. The plateau from 2005–2025 suggests the system stabilized at a new equilibrium where extraction and suppression are sustained but do not intensify further.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 institutional seat, the RBIO framework is seen as genuine coordination machinery that constrains unilateral action and legitimates necessary enforcement — selectivity is regrettable limitation, not design. From the targeted sovereign seat, the same framework is seen as a hierarchical system that reserves decision-making to the powerful and extracts compliance costs while claiming universality. From the institutional apparatus seat (UNSC, courts), the system is seen as legitimate arbiter machinery that distributes authority according to principle; the apparatus benefits from remaining structured as a neutral procedures-based space even though the P5 veto ensures that certain parties' interests are structurally privileged. The engine should compute these seats as having different directionalities: P5 and institutional apparatus near the beneficiary end (d ~ 0.2–0.35), targeted sovereigns near the target end (d ~ 0.8–0.9), civilian populations even higher (d ~ 0.9), sovereignty advocates in the middle but excluded (d ~ 0.6). The liberal reading asserts these differences are not fundamental; the structural data (veto power, exit options, enforcement selectivity) establish them.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states: power = institutional, exit = arbitrage (can opt out of specific enforcement actions or UNSC procedures but remain P5), directionality = beneficiary (set agenda, control veto, benefit from legitimacy frame). Directed χ approaches subsidization (negative extraction). Enforcement contractors: power = organized, exit = mobile (can pursue other contracts), directionality = beneficiary (gain contracts and legitimacy from RBIO framing). Directed χ is damped subsidy. Targeted sovereigns: power = moderate, exit = trapped (cannot leave international system), directionality = target (bear intervention and sanctions costs, excluded from amendment). Directed χ is amplified extraction. Civilian populations: power = powerless, exit = trapped, directionality = target (highest possible). Directed χ is maximum extraction. Sovereignty advocates: power = powerful, exit = constrained (can resist but not reshape), directionality = asymmetric (they challenge the frame but are excluded from decision-making, so d ~ 0.55–0.65, between beneficiary and target, but structurally excluded). The P5 veto creates the directionality asymmetry: beneficiaries can block amendment; targets cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII coordination on norms without great-power war) remains contested rather than clearly resolved or obsolete. The liberal reading insists it is live and that the RBIO framework solves it; the hegemonic extraction reading insists it was solved by Cold War deterrence and the framework now serves rent collection; the sovereignty reading insists it was never the real problem and was always a pretext for hierarchy. The measured theater_ratio (0.48 and rising from 1945 to 2005) indicates that an increasing share of institutional activity is devoted to narrating and defending the reading itself rather than to the coordination function. This is consistent with a constraint approaching mandatrophy: the original function (coordination on norms) is increasingly overshadowed by the extraction function (P5 authority maintenance) and the legitimation function (defending the reading against challengers). However, the theater ratio stabilizes after 2005, suggesting the system found a stable equilibrium where both functions persist. The resistance measurement (0.74) is high, indicating substantial active challenge to the reading from sovereignty advocates and from targeted sovereigns' pushback against selectivity. This sustained resistance, combined with the high theater ratio, is consistent with a tangled_rope rather than a pure snare: the coordination function remains real enough that beneficiaries and parts of the affected population still support it (human rights advocates, peacekeeping-dependent states), but the extraction function is substantial and the reading's claim to universality and consent is increasingly threadbare. Mandatrophy is not yet structurally complete, but the trajectory is toward it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_mechanism,
    'Is enforcement selectivity a structural feature of the RBIO framework (designed to privilege P5 interests), a capacity problem (insufficient resources and political will to enforce universally), or a path-dependent historical artifact (selective enforcement in early cases locked in precedent)?',
    'Counterfactual analysis: if resources and political will were unlimited, would enforcement become universal (capacity problem) or would P5 states continue to choose selective enforcement (structural design)? Historical case comparison across different institutional contexts (League of Nations, regional bodies, post-colonial governance) to identify whether selectivity is endemic to hierarchical systems.',
    'Capacity diagnosis supports the liberal reading and predicts selectivity decreases with institutional maturity and resources. Structural design diagnosis supports the hegemonic extraction reading and predicts selectivity persists regardless of resources. The classification hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Root cause of enforcement selectivity: capacity, design, or path-dependency').

omega_variable(
    p5_veto_as_amendment_barrier,
    'Does the P5 veto structure make amendment of RBIO principles practically impossible (contradicting the reading''s claim of revisability), or merely difficult and slow (supporting the reading''s claim that revision is possible through legitimate multilateral process)?',
    'Historical analysis of amendment attempts (treaty revisions, UNSC expansion proposals, Charter amendment efforts) and their outcomes. Structural analysis: what would be required for amendment to succeed despite P5 resistance? If the answer is ''nothing short of great-power consensus or hegemonic transition,'' then amendment is practically blocked.',
    'If amendment is practically blocked, the reading''s central claim (the system is revisable through legitimate process) is false, and the constraint reclassifies toward snare. If amendment is merely difficult, the reading remains credible and the constraint remains tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p5_veto_as_amendment_barrier, empirical, 'Whether P5 veto makes RBIO amendment practically impossible').

omega_variable(
    universalism_claim_vs_veto_structure,
    'Is there a fundamental logical contradiction between the liberal reading''s claim that RBIO norms are universal and consent-based and the structural fact that non-P5 states cannot amend or block enforcement decisions?',
    'Philosophical analysis: does consent require the ability to refuse or amend the arrangement? If yes, the veto structure means non-P5 states do not truly consent to the system. If consent can be understood as acceptance of a procedure (even if the procedure privileges some parties), the claim survives.',
    'A logical contradiction would strengthen the hegemonic extraction and sovereignty-maximalist readings. If the liberal reading can sustain a concept of consent compatible with veto-blocked amendment, the reading survives but at the cost of a thinner concept of consent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universalism_claim_vs_veto_structure, conceptual, 'Logical compatibility of universalism + consent with P5 veto structure').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who are the primary beneficiaries of the RBIO framework? The liberal reading emphasizes beneficiaries as states cooperating on norms and populations protected by humanitarian enforcement. But the structural data (P5 veto, selective enforcement, contractor enrichment) suggest beneficiaries are P5 states and enforcement contractors, not the broader international community.',
    'Benefit-flow analysis: who actually collects from the system? UNSC authorization confers legitimacy on intervening states (P5 capture). Sanctions regimes extract from targeted civilians (third-party harm). Reconstruction contracts flow to contractor firms (organized benefit capture). Humanitarian enforcement legitimacy benefits human rights advocates and intervening states (concentrated benefits). Compare against: do non-intervening states, targeted sovereigns, and civilian populations benefit?',
    'If primary beneficiaries are narrow (P5, contractors, advocacy coalition), the constraint is extractive and the tangled_rope classification is stable. If broader beneficiaries exist (the international system as a whole benefits from coordination on norms, civilian populations benefit from atrocity prevention), the classification might shift toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Breadth and concentration of beneficiaries under RBIO framework').

omega_variable(
    sovereign_consent_fiction,
    'The liberal reading frames RBIO norms as consent-based because states can theoretically ratify treaties and participate in General Assembly votes. But some states never ratified key instruments, are coerced into compliance by threat of intervention, or face sanctions for non-compliance with unratified norms. Is the consent fiction sustainable or has it collapsed?',
    'Forced-choice analysis: states that did not ratify a treaty but face sanctions for violation did not consent. Count the proportion of enforcement actions against non-ratifying states. If substantial, the consent-basis claim is empirically false.',
    'If consent fiction is collapsed, the reading''s central legitimacy claim fails. The constraint would reclassify toward snare with a tattered legitimacy wrapper. If consent can be sustained as tacit acceptance of the international legal order (even for non-ratifying states), the reading survives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereign_consent_fiction, empirical, 'Whether RBIO consent basis survives enforcement against non-ratifying states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement_basis(rbio_tr_t1945, observed).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1965, 0.31).
narrative_ontology:measurement_basis(rbio_tr_t1965, observed).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement_basis(rbio_tr_t1990, observed).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement_basis(rbio_tr_t2005, observed).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement_basis(rbio_tr_t2015, observed).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(rbio_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(rbio_be_t1945, observed).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement_basis(rbio_be_t1965, observed).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement_basis(rbio_be_t1990, observed).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(rbio_be_t2005, observed).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(rbio_be_t2015, observed).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(rbio_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement_basis(rbio_su_t1945, observed).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1965, 0.58).
narrative_ontology:measurement_basis(rbio_su_t1965, observed).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1990, 0.67).
narrative_ontology:measurement_basis(rbio_su_t1990, observed).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement_basis(rbio_su_t2005, observed).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(rbio_su_t2015, observed).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(rbio_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__liberal_institutional_reading, 0.18).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% The RBIO practice norm complex decomposes into three structurally distinct constraint stories representing three different readings of the same contested institutional framework. This reading (liberal institutional) diagnoses selectivity as capacity problem and asserts the system is universalist and revisable. The hegemonic extraction reading diagnoses selectivity as intentional and the system as frozen by P5 veto. The sovereignty-maximalist reading diagnoses both readings as illegitimate interference. Each reading instantiates a different ε, beneficiary/victim structure, and claimed type. They share a kernel (the RBIO institutional arrangement and norms) but diverge on its legitimacy foundation and function. Network links connect them because the empirical falsification of one reading's key claims (e.g., if amendment proves practically impossible) creates pressure for reclassification across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
