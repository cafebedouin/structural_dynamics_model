% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Recognition Requirement for Statehood
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention (1933) established four objective criteria for
 *   statehood: defined territory, permanent population, effective government,
 *   and capacity to conduct international relations. However, the Convention
 *   also requires recognition by the existing community of states as the
 *   mechanism for admission. This constraint instantiates the CONSTITUTIVE
 *   READING of that kernel — the reading that recognition by existing states
 *   is not merely declarative (announcing a legal fact that already exists)
 *   but CONSTITUTIVE (creates statehood as a legal fact). Under this reading,
 *   a polity can possess all four objective criteria and still lack statehood
 *   until recognized. The constraint sits in the contested kernel space
 *   alongside the declaratory reading (objective criteria establish statehood
 *   automatically) and the hybrid reading (objective criteria plus normative
 *   legitimacy tests are required). The constitutive reading concentrates
 *   power in the existing state community and creates a structural veto over
 *   new state creation, making it substantially extractive for unrecognized
 *   polities and independence movements. The claim/metric gap is intentional:
 *   the constraint is CLAIMED as tangled_rope (it does coordinate the
 *   international system AND extract through recognition veto), and the
 *   metrics confirm asymmetric extraction and active enforcement. The
 *   measurement series shows extractiveness and suppression rising toward
 *   stabilization by t=30, with theater ratio moderating as the political
 *   nature of recognition decisions becomes more visible over time.
 *
 * KEY AGENTS:
 *   - existing_state_community: Institutional beneficiary — collectively controls recognition and retains veto over new state creation
 *   - unrecognized_polities: Powerless victims — meet objective criteria but cannot achieve legal statehood without recognition
 *   - independence_movements: Moderate-power payers — face dual barrier of territorial claims and recognition veto
 *   - parent_states: Powerful beneficiaries — retain veto over territories seeking independence
 *   - great_powers: Institutional beneficiaries — use recognition as geopolitical leverage
 *   - international_law_scholars: Observers — debate which reading of the kernel is correct
 *   - international_courts: Institutional observers — cannot adjudicate recognition disputes because claimants lack standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.71).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Recognition Requirement for Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, 'a64eb9e3-f988-42a6-83e3-6da05bd2b790').
narrative_ontology:cs_kernel_codification('a64eb9e3-f988-42a6-83e3-6da05bd2b790', formalized).
narrative_ontology:cs_authority_grounding('a64eb9e3-f988-42a6-83e3-6da05bd2b790', extraction).
narrative_ontology:cs_interpretation_layer_present('a64eb9e3-f988-42a6-83e3-6da05bd2b790').
narrative_ontology:cs_reading_relation('a64eb9e3-f988-42a6-83e3-6da05bd2b790', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('a64eb9e3-f988-42a6-83e3-6da05bd2b790', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a64eb9e3-f988-42a6-83e3-6da05bd2b790', foundational, recognition_constitutes_legal_status).
narrative_ontology:cs_axiom_status(recognition_constitutes_legal_status, holdable).
narrative_ontology:cs_axiom_grounding('a64eb9e3-f988-42a6-83e3-6da05bd2b790', recognition_constitutes_legal_status, conventional).
narrative_ontology:cs_axiom('a64eb9e3-f988-42a6-83e3-6da05bd2b790', foundational, existing_states_possess_admission_prerogative).
narrative_ontology:cs_axiom_status(existing_states_possess_admission_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('a64eb9e3-f988-42a6-83e3-6da05bd2b790', existing_states_possess_admission_prerogative, deontological).
narrative_ontology:cs_reference_frame('a64eb9e3-f988-42a6-83e3-6da05bd2b790', montevideo_recognition_adjudication).
narrative_ontology:cs_drift_state('a64eb9e3-f988-42a6-83e3-6da05bd2b790', contemporary_de_facto_state_proliferation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a64eb9e3-f988-42a6-83e3-6da05bd2b790', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_state_community).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, independence_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, de_facto_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, parent_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, great_powers).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, state_sovereignty_as_social_fact).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, international_legal_authority_vests_in_existing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively possess the power to recognize or withhold recognition from claimant polities. Controls access to UN membership, treaty participation, diplomatic immunity, and international finance. Administers the recognition criterion through bilateral diplomacy, UN voting, and institutional practice. Benefits from the rule because it preserves the prerogative to veto new states on political grounds (humanitarian concerns, regional stability, alignment) while presenting the decision as objective legal assessment.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_state_community, agenda_setter,
    institutional, generational, analytical, universal).

% Meet the Montevideo Convention's objective criteria (defined territory, permanent population, effective government, capacity to conduct international relations) but lack recognition from enough existing states to achieve statehood status. Cannot participate in the UN, cannot sign treaties, cannot borrow from international institutions, cannot enforce property claims in international courts, cannot participate in the rules-making that could change the rule. Their status is administered to them; they have no voice in whether the rule applies.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, biographical, trapped, universal).

% Seek statehood for territories under foreign control or disputed sovereignty. Face the dual barrier that existing states whose territory they might claim have veto power over their recognition (the parent state can block). Recognition becomes a bargaining chip in territorial negotiation rather than a pure legal matter. Even if they establish functioning government and territorial control, they cannot force recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, independence_movements, payer,
    moderate, biographical, constrained, regional).

% Retain veto over the statehood of territories they administer. The rule that recognition is constitutive gives them a structural power they would not have under a declaratory system. They can block independence movements indefinitely by withholding recognition, even if those movements control territory and population.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, parent_states, beneficiary,
    powerful, generational, analytical, regional).

% Use recognition as a geopolitical tool to reward alignment and punish non-alignment. The constitutive rule allows recognition decisions to rest on political criteria (whether the regime is friendly, whether recognition serves regional balance) while invoking legal assessment as the justification. Benefit from the discretion embedded in the recognition prerogative.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, great_powers, beneficiary,
    institutional, generational, analytical, universal).

% Debate which reading of the Montevideo Convention is correct. The constitutive reading scholars argue recognition creates statehood; the declaratory reading scholars argue objective criteria determine it; hybrid scholars argue recognition plus legitimacy tests are required. The scholarship itself becomes a contested framing of what the constraint IS.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_law_scholars, observer,
    moderate, biographical, analytical, global).

% Have limited jurisdiction over recognition disputes because courts exist only for recognized states. Kosovo, Palestine, and Somaliland have no standing to challenge their non-recognition in the ICJ. Courts can only adjudicate disputes among recognized parties, which means they cannot rule on the recognition rule itself.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_courts, observer,
    institutional, generational, analytical, global).

% Communities and movements in unrecognized polities would object to the constitutive rule if given voice — they would argue that meeting objective criteria should establish statehood without needing permission from powers outside the region. They are structurally excluded from the recognition decision by the rule itself.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, excluded_regional_voices, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, existing_state_community).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, centrally-administered criterion for distinguishing states from non-states in international law: recognition by the existing state community creates an objective, consensual boundary that prevents proliferation of conflicting sovereignty claims and maintains a stable roster of legal persons in the system.
% TRANSFER_FUNCTION: Transfers the power to determine statehood from objective facts (territory, population, government) to the collective political will of existing states. Existing states receive the authority to admit or exclude new entrants; unrecognized polities lose control over their own legal status even when they possess all the functional attributes of statehood.
% ABSENT_VOICES: Unrecognized polities themselves — they would object to having their status determined by entities outside their community, arguing that objective criteria should be sufficient. Independence movements, de facto states, and regional actors who benefit from the status quo but lack great-power support would also contest the rule if given a vote in its formation or amendment.
% DISAPPEARANCE_RATIONALE: If the constitutive recognition requirement vanished and statehood became automatic upon meeting objective criteria (the declaratory reading), dozens of de facto states would immediately enter the international system as legal persons: Kosovo, Somaliland, Northern Cyprus, Transnistria, South Ossetia, Abkhazia, and others. UN membership, treaty participation, and international court access would reshape overnight. The existing state community would lose its veto power and would reorganize around a more fragmented international legal order.
% FOUNDING_PROBLEM: Early 20th-century international law lacked clear criteria for statehood, leading to contested claims and unstable succession. The Montevideo Convention (1933) was written to create objective tests (territory, population, government, capacity for relations) and to require recognition by the existing state community as the mechanism for adjudicating disputed claims.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars from both constitutive and declaratory traditions acknowledge the founding problem existed. However, they dispute whether recognition by the state community IS the solution or merely ONE reading of the solution. Declaratory-reading scholars argue that objective criteria alone suffice and that requiring recognition conflates legal status with political discretion. Unrecognized polities and their advocates argue the founding problem has been solved functionally (de facto states DO govern, DO conduct relations, DO maintain territorial control) and the recognition rule now blocks justified statehood claims rather than preventing chaos.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constitutive reading transfers the power to determine statehood from objective functional criteria to the political will of existing states. A polity can meet all four Montevideo criteria and still be denied statehood by great-power veto or regional opposition. Suppression is higher still (0.71) because unrecognized polities have no institutional voice in the recognition process — they cannot petition the UN as states, cannot participate in treaty-making, cannot challenge non-recognition in international courts (which have jurisdiction only over recognized states). Accessibility collapse is high (0.73) because once the constitutive rule is understood, the only alternative path to legal statehood IS recognition by existing states; there is no mechanism for objective criteria alone to establish status. Theater ratio moderates at 0.42 because while recognition decisions are justified as legal assessments of the Montevideo criteria, the actual practice increasingly reflects political discretion (Kosovo recognized by Western states but not Russia; Palestine recognized by much of the world but not the US; Somaliland governed effectively for 30 years but remains unrecognized). The temporal series shows extraction accumulating as the gap between functional statehood (de facto) and legal statehood (recognized) widens — more polities exist in this gap over time, and each adds to the suppression profile of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is the core of this constraint's structure. From the agenda-setter (existing state community) seat, the constraint appears as rope: it coordinates statehood recognition, prevents chaos from proliferating sovereignties, and maintains a stable international legal order. The fact that existing states benefit is incidental to the coordination function they perform. From the payer seat (unrecognized polities), the same structure appears as snare: the coordination is a cover story, the rule is enforced to protect the power of existing states to exclude new entrants, and the beneficiaries are the states whose interests the system serves. The engine computes this divergence from the structural data: the payer seat has trapped exit, zero voice, and no leverage, while the agenda-setter seat has analytical exit and controls the rules. The divergence is not a bug — it is the fingerprint of a tangled constraint where one party genuinely coordinates and another is extracted from through the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The existing state community sits at d near 0.0 (full beneficiary) — the rule concentrates authority in them and they control its application. Unrecognized polities sit at d near 1.0 (full target) — they are subject to the rule, have no voice in its administration, and bear the extraction directly. Independence movements sit at d near 0.9 (high target) — they face the dual barrier and have constrained rather than trapped exit (they can continue as de facto states or revert to colonial/parent-state control, but cannot force recognition). Parent states sit at d near 0.1 (beneficiary) — they benefit from the veto and face no extraction from the rule itself. Great powers sit at d near 0.2 (beneficiary) — they use recognition discretion but are not trapped by it. The directionality divergence between the institutional agenda-setter and the powerless victims is extreme and explains why the same constraint would compute as rope-like or tangled-rope-like from different seats: the existing states experience the rule as genuine coordination (objective criteria plus consensual admission), while unrecognized polities experience it as enforcement of their exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The constitutive reading avoids mandatrophy by anchoring to a live founding problem and a contested legitimacy claim. The founding problem (preventing chaos from conflicting statehood claims) remains live in the constitutive reading's view — unrecognized polities are precisely the source of potential conflict that recognition-based adjudication is meant to prevent. However, the declaratory reading challenges this: they argue the founding problem is solved functionally (de facto states DO maintain order, DO conduct relations, DO have populations) and that the recognition rule now creates instability rather than preventing it by denying legal status to functioning polities. This contest means the constraint does not meet mandatrophy's silence criterion — the founding problem's status is actively disputed, not forgotten. The theater ratio's rise from 0.28 to 0.42 suggests increasing performativity (decisions justified as legal assessments while actually reflecting geopolitics), but this is consistent with a tangled rope under pressure, not a piton. A true piton would show theater dominance (>0.65) with flat extraction (beneficiaries don't care enough to maintain it actively); this constraint shows both rising extraction and rising theater, which is the profile of an extractive arrangement defending itself as its cover story wears thin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_boundary,
    'Is statehood a social/political fact (created by recognition, constitutive reading) or a legal fact (determined by objective criteria, declaratory reading)?',
    'Examine state practice: do states that meet objective criteria but lack recognition exercise the rights and bear the duties of statehood? Do de facto states behave legally even without recognition? Does the international system treat them as states in practice while denying recognition formally?',
    'If de facto states function legally despite non-recognition (conduct diplomacy, sign treaties, participate in dispute resolution), the constitutive reading''s authority claim weakens. If they remain excluded from legal participation despite functional statehood, the constitutive reading is sustained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_boundary, empirical, 'Whether the legal status of statehood is determined by existing-state recognition or by objective functional criteria.').

omega_variable(
    veto_power_as_coordination_vs_extraction,
    'Is the existing state community''s veto power over recognition a necessary coordination mechanism (preventing chaos from proliferating sovereignties) or a mechanism for extracting power (blocking independence movements that threaten existing states)?',
    'Analyze recognition patterns: do great powers block recognition based on objective criteria failures, or based on geopolitical alignment? Do they recognize de facto states that meet criteria, or only those that serve strategic interests? Does the recognition rule prevent instability or create it by denying legal status to functioning polities?',
    'If veto is exercised primarily on geopolitical grounds rather than objective criteria, the coordination framing is a cover story and the extraction framing is correct. If veto prevents genuine conflicts from proliferating, the coordination framing is substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_power_as_coordination_vs_extraction, empirical, 'Whether the veto mechanism serves coordination or extraction.').

omega_variable(
    kernel_reading_foreclosure,
    'Can the constitutive reading and the declaratory reading coexist within a single international legal authority, or does adopting one require rejecting the other?',
    'Examine whether a legal system could hold that (1) statehood requires recognition by existing states AND (2) objective criteria alone determine statehood. Can both claims be true simultaneously, or are they logically incompatible?',
    'If they are logically incompatible (foreclosure), the kernel contest is a genuine disjunction where accepting one reading means rejecting the other. If they can coexist (perhaps distinguishing between legal status and practical capacity), the kernel contest is an influence relationship rather than a foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the constitutive and declaratory readings logically foreclose each other or merely conflict.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of unrecognized polities purely structural (barriers to treaty participation, court access, UN membership) or partly internalized (unrecognized polities'' self-concept shaped by non-recognition, reducing resistance to the rule)?',
    'Track whether unrecognized polities that gain partial recognition (EU-level diplomatic relations, humanitarian organization membership) show increased resistance to full non-recognition, or whether suppression persists through internalized acceptance of inferior status.',
    'If suppression is purely structural, removing barriers (opening UN participation, lowering recognition thresholds) would increase resistance. If partly internalized, the constraint''s suppression power persists even after barriers fall — the targets carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of unrecognized polities is structural barriers or internalized status hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(mont_tr_t0, observed).
narrative_ontology:measurement(mont_tr_t5, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(mont_tr_t5, observed).
narrative_ontology:measurement(mont_tr_t10, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(mont_tr_t10, observed).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(mont_tr_t15, observed).
narrative_ontology:measurement(mont_tr_t20, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(mont_tr_t20, observed).
narrative_ontology:measurement(mont_tr_t25, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(mont_tr_t25, observed).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(mont_tr_t30, observed).
narrative_ontology:measurement(mont_tr_t40, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(mont_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(mont_be_t0, observed).
narrative_ontology:measurement(mont_be_t5, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 5, 0.57).
narrative_ontology:measurement_basis(mont_be_t5, observed).
narrative_ontology:measurement(mont_be_t10, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(mont_be_t10, observed).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(mont_be_t15, observed).
narrative_ontology:measurement(mont_be_t20, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(mont_be_t20, observed).
narrative_ontology:measurement(mont_be_t25, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(mont_be_t25, observed).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(mont_be_t30, observed).
narrative_ontology:measurement(mont_be_t40, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(mont_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(mont_su_t0, observed).
narrative_ontology:measurement(mont_su_t5, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(mont_su_t5, observed).
narrative_ontology:measurement(mont_su_t10, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(mont_su_t10, observed).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(mont_su_t15, observed).
narrative_ontology:measurement(mont_su_t20, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(mont_su_t20, observed).
narrative_ontology:measurement(mont_su_t25, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(mont_su_t25, observed).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(mont_su_t30, observed).
narrative_ontology:measurement(mont_su_t40, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(mont_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__constitutive_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested montevideo_statehood_criteria kernel. The constitutive reading holds that recognition by existing states CREATES statehood as a legal fact. The declaratory reading holds that objective Montevideo criteria alone establish statehood. The hybrid reading requires objective criteria plus normative legitimacy tests. Each reading is a separate constraint story with distinct ε, beneficiary/victim structure, and classification, linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
