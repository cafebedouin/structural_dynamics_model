% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: Military as Constitutional Guardian (Military Custodian Reading)
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   This constraint instantiates the military custodian reading of the July
 *   Charter — the constitutional arrangement that emerged from
 *   post-revolutionary state-building. Under this reading, the Charter
 *   ratifies the military as a permanent institutional guardian with veto
 *   authority over civilian political contestation, justified by the need for
 *   stability during state transition. This reading interprets ambiguous
 *   constitutional language about military roles and national security as
 *   granting the military standing authority to judge whether civilian
 *   political activity threatens state stability and to intervene
 *   accordingly. Civilian political parties operate within military-defined
 *   bounds; autonomous social movements (students, civil society) face
 *   suppression when they contest military authority; ordinary citizens are
 *   excluded from amending the constraint that governs them. The measurement
 *   series shows extractiveness rising from 0.68 to 0.82 over the interval as
 *   the military's initial emergency guardianship hardens into permanent
 *   institutional role. Theater ratio rises from 0.25 to 0.41, indicating
 *   growing performativity: early revolutionary legitimacy narratives
 *   ('temporary guardian until democracy stabilizes') persist rhetorically
 *   even as permanent institutional interests become visible. Suppression
 *   requirement intensifies from 0.72 to 0.88 as autonomous mobilization
 *   grows and requires increasing coercive maintenance.
 *
 * KEY AGENTS:
 *   - military_institution: primary beneficiary and agenda-setter (institutional power, arbitrage exit); sets the bounds of permissible political activity and collects institutional autonomy
 *   - civilian_political_parties: primary target (moderate power, constrained exit); operate within military-defined bounds, cannot challenge military veto without existential risk
 *   - student_movement: secondary target (powerless, trapped); face detention and violence for autonomous organizing outside military channels
 *   - autonomous_civil_society: secondary target (organized power, constrained exit); operate under security review, defunding and deregistration threats
 *   - revolutionary_council: secondary beneficiary (institutional power, arbitrage exit); provides appearance of collective deliberation, legitimizes military decisions
 *   - international_allies: tertiary beneficiary (institutional power, arbitrage exit); geopolitical interests align with military-controlled stability
 *   - ordinary_citizens: excluded (powerless, trapped); cannot participate in constitutive amendment or formal contestation of military veto
 *   - secular_democratic_reading_advocates: excluded (moderate power, trapped); marginalized from official Charter interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.82).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.88).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "Military as Constitutional Guardian (Military Custodian Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '477d8577-315b-4b15-92ea-d458d1cda79d').
narrative_ontology:cs_kernel_codification('477d8577-315b-4b15-92ea-d458d1cda79d', formalized).
narrative_ontology:cs_authority_grounding('477d8577-315b-4b15-92ea-d458d1cda79d', extraction).
narrative_ontology:cs_interpretation_layer_present('477d8577-315b-4b15-92ea-d458d1cda79d').
narrative_ontology:cs_reading_relation('477d8577-315b-4b15-92ea-d458d1cda79d', july_charter_sovereign_legitimacy__secular_democratic_reading, coexists_with).
narrative_ontology:cs_reading_relation('477d8577-315b-4b15-92ea-d458d1cda79d', july_charter_sovereign_legitimacy__guided_nationalism_reading, influences).
narrative_ontology:cs_axiom('477d8577-315b-4b15-92ea-d458d1cda79d', foundational, military_institutional_guardianship_necessary).
narrative_ontology:cs_axiom_status(military_institutional_guardianship_necessary, holdable).
narrative_ontology:cs_axiom_grounding('477d8577-315b-4b15-92ea-d458d1cda79d', military_institutional_guardianship_necessary, instrumental).
narrative_ontology:cs_axiom('477d8577-315b-4b15-92ea-d458d1cda79d', foundational, civilian_democratic_capacity_insufficient_for_transition).
narrative_ontology:cs_axiom_status(civilian_democratic_capacity_insufficient_for_transition, holdable).
narrative_ontology:cs_axiom_grounding('477d8577-315b-4b15-92ea-d458d1cda79d', civilian_democratic_capacity_insufficient_for_transition, empirically_contingent).
narrative_ontology:cs_reference_frame('477d8577-315b-4b15-92ea-d458d1cda79d', post_revolutionary_emergency_state_stabilization).
narrative_ontology:cs_drift_state('477d8577-315b-4b15-92ea-d458d1cda79d', contemporary_state_maturity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('477d8577-315b-4b15-92ea-d458d1cda79d', '2026-06-11T14:32:15Z').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_institution).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_civil_society).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, revolutionary_council).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, international_allies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Charter as granting it veto authority over civilian political contestation in the name of national stability. Sets the bounds of permissible political activity, reviews legislation that might 'destabilize' the state, administers the national security apparatus, and can intervene directly in government formation. Claims the role is emergency guardianship until 'transition to democracy' is 'secure' — a transition date that recedes continuously. Collects institutional autonomy, budget protection from civilian oversight, and influence over state policy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_institution, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate within Charter-defined boundaries that require military approval for major policy directions. Cannot challenge the military's institutional role or its security judgments without facing dismissal, arrest, or party dissolution. Have nominal legislative power but real decision authority rests with military-controlled security apparatus. Exit option is leaving politics entirely or operating underground, both costly and risky.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_political_parties, payer,
    moderate, biographical, constrained, national).

% Mobilizes for autonomous political voice and challenge to military authority. Treated as destabilizing by the security apparatus. Face arrest, detention without trial, and violent suppression when organizing outside military-controlled channels. Cannot exit from the state's security umbrella and have no formal channels to contest military authority. Political organizing itself becomes criminalized as 'threatening stability.'
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, biographical, trapped, national).

% NGOs, labor organizations, and community groups operate under security review. Must avoid activities classified as political organizing or criticism of the state's security posture. Can be defunded, deregistered, or leaders detained if their work crosses the military's implicit red lines. Constrained between providing services (allowed) and advocacy (prohibited).
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_civil_society, payer,
    organized, biographical, constrained, national).

% Composed of military and civilian figures chosen by the military. Legitimizes military decisions through the appearance of collective deliberation. Members benefit from access to power but lack independent authority; their role is ratification, not decision-making. Provides cover for military rule by naming it constitutional governance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, revolutionary_council, beneficiary,
    institutional, generational, arbitrage, national).

% Geopolitical allies view military-controlled stability as preferable to ideological uncertainty or popular mobilization that might produce unfriendly governments. Provide military aid, intelligence sharing, and diplomatic cover. Have no direct voice in Charter interpretation but their security interests align with military institutionalization.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_allies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, international_allies, observer).

% Would participate in constituent assembly or popular ratification of constitutional structures under autonomous democratic procedures. Are excluded from Charter legitimation — the document is presented as already-ratified, closed to popular amendment. Cannot formally object to military guardianship even if they oppose it. Their exclusion from the committing act is the reading's structural foundation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, ordinary_citizens, excluded,
    powerless, biographical, trapped, national).

% Argue that the Charter should be read as mandating civilian democratic control and military subordination. Are systematically marginalized from official Charter interpretation. Their reading is treated as naive or destabilizing. Have no institutional platform to contest this reading's ascendancy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, secular_democratic_reading_advocates, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, military_institution).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The military custodian reading coordinates state stability and national security by centralizing authority in a professional military institution deemed competent to manage security threats and transitions. It solves a coordination problem: after revolutionary upheaval, who directs the state through the dangerous transition period to prevent collapse or competing seizures of power?
% TRANSFER_FUNCTION: Moves political autonomy from civilian populations and parties to the military institution. Citizens and parties transfer veto authority over legislation, party formation, and political organizing to military security apparatus. The military transfers its ability to act as an impartial professional institution to itself as a factional political actor with permanent institutional interests.
% ABSENT_VOICES: Ordinary citizens who might reject military guardianship if they had been included in the constitutive act. Secular democratic reading advocates who contest the military custodian interpretation of the Charter. Regional populations or minorities whose security interests diverge from military-defined stability. Revolutionary constituencies whose struggle produced the transition but are now constrained by military-set rules.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if the Charter were genuinely amended to place the military under civilian democratic control — the military's veto authority over legislation would evaporate, political parties could organize freely, the student movement could mobilize without detention threats, and state policy would shift to reflect electoral majority rather than military security judgments. The entire institutional geography of post-revolutionary governance would reorganize.
% FOUNDING_PROBLEM: Revolutionary upheaval produced a power vacuum and competing claimants to state authority. The founding problem: who stabilizes the state during the dangerous transition from revolutionary ferment to stable governance? The reading asserts the military is the only institution with the hierarchy, training, and impartiality to manage the transition without state collapse or permanent factional rule.
% FOUNDING_PROBLEM_CORROBORATION: The military institution and allied civilian officials attest the founding problem is live and ongoing — state stability remains fragile, threats remain high, civilian politicians remain naive about security exigencies. Secular democratic advocates, student movements, and regional analysts attest the founding problem was specific to the post-revolutionary moment and is now obsolete; the military's permanent institutional role is rent-seeking disguised as guardianship. International human rights monitors and foreign governments with democratic commitments corroborate the second reading: the transition window has closed, permanent military veto is incompatible with democratic governance, and the constraint persists as institutional extraction.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the military collects political autonomy and institutional veto authority without corresponding accountability; the beneficiary (military) is a single unified institutional actor with clear material interests in perpetuating the constraint. Suppression is higher still (0.88) because the constraint's persistence depends on active coercive maintenance: civilian parties must be prevented from organizing autonomously, students must be deterred from mobilizing, civil society must be constrained within service delivery. The constraint would not persist without active enforcement against the excluded and subordinated stakeholders. Theater ratio is moderate-high (0.41) and rising, indicating that performative narrative (emergency guardianship, temporary transition, stability rationale) occupies an increasing share of enforcement activity relative to actual security function. The military's initial claim to temporary custodianship was plausible in the post-revolutionary moment; as decades pass and 'transition to democracy' recedes indefinitely, the narrative becomes thinner and the extractive institutional interest becomes more visible. Suppression requirement rises steeply (0.72 to 0.88) because autonomous mobilization increases as the emergency-transition justification wears thin, requiring the military to intensify coercive maintenance to preserve veto authority. Accessibility collapse is high (0.79) because civilians cannot exit the state or formally amend the constraint that binds them; alternatives to military-custodian governance are foreclosed by constitutional design, not merely by military preference. Resistance is moderate-high (0.62) and likely rising: student movements, civil society organizations, and secular democratic advocates actively contest military veto, but lack institutional platforms for effective challenge.
 *
 * PERSPECTIVAL GAP:
 *   The military institution and allied civilian officials experience this constraint as benign or beneficial coordination: a professional institution managing state security and guiding transition to democracy. They do not contest that the military has veto authority; they dispute only whether that authority has become permanent (they say it remains transitional and contingent on external threats). From this seat, the constraint is genuine coordination overlaid with temporary functional authority. Civilian political parties experience the constraint as extractive oppression with thin legitimation: they have lost autonomous voice, face existential threats if they challenge military authority, and the promised 'transition to democracy' indefinitely recedes. Their seat experiences the constraint as a snare — coerced participation in military-brokered politics, with no exit except political silence. Student movements and civil society experience the constraint as purely suppressive: they face detention and violence for autonomous organizing, and have no participation at all in the military-custodian framework. Their exclusion is not negotiated coordination but active repression. The secular democratic reading advocates experience the constraint as a constitutional usurpation: the Charter language about national security and military roles is being read to grant permanent veto authority that the text does not support, and alternative readings that would subordinate the military to civilian democratic control are being marginalized from official interpretation. The perspectival gap arises from structural position (who benefits, who pays) and from epistemically divergent readings of the same Charter text. The engine computes per-seat type from the structural data; the reading-divergence is an omega variable.
 *
 * DIRECTIONALITY LOGIC:
 *   The military institution is the structural beneficiary (d near 0.0): it gains autonomy, budget insulation, veto authority, and political influence without bearing the costs of democratic accountability. Its exit option is arbitrage — it could subordinate itself to civilian control, but doing so is not constrained or trapped; the choice is active institutional strategy. The military as a seat experiences this constraint as coordination (protecting the state through professional security management) layered over extraction (collecting permanent veto authority). From the military's seat, d is low; the constraint subsidizes military interests. Civilian political parties are targets (d near 0.8–0.9): they lose autonomy, operate under veto threat, face party dissolution if they challenge military authority, and cannot exit without abandoning electoral politics entirely. Their exit is constrained (can organize underground, but the cost is near-total exclusion). From the parties' seat, d is high; the constraint extracts from them. Students and civil society sit at d ≈ 0.85–0.95: they are actively suppressed, face detention and violence, cannot organize autonomously, and have no institutional platforms for contestation. Their exit is identity_locked and trapped (they cannot leave the nation, and political voice is part of their social identity). Ordinary citizens are excluded from the analysis (d is undefined for non-participants in the constraint), but if they were to participate they would experience d ≈ 0.9 — no veto over their own governance, no exit from the constitutional arrangement that binds them. The revolutionary council sits at d ≈ 0.1–0.2: they benefit from access to power and legitimacy by association, but lack independent authority; the military retains all real decision-making. The seated divergence (beneficiary vs. target seats compute different types) is the analytic core: from the military's seat the constraint computes as coordination (rope) with some extraction overlay; from the target seats it computes as enforced extraction (snare) with thin coordination narrative. The engine's per-seat classification captures this divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The military custodian reading of the Charter faces a mandatrophy challenge: the founding problem (state instability during post-revolutionary transition) has either been solved or is being indefinitely deferred. If the problem is solved, the military's permanent veto authority is obsolete — the constraint persists as rent-seeking disguised as guardianship. If the problem is live and ongoing, the military must continuously demonstrate that civilian political parties and autonomous movements pose genuine security threats; as decades pass, the threat narrative becomes implausible and performative. The measurement series shows extractiveness rising (0.68 to 0.82) while suppression requirement rises steeply (0.72 to 0.88), indicating that the constraint's persistence increasingly depends on active coercion rather than consensus about founding problem validity. Theater ratio rises from 0.25 to 0.41, indicating growing performativity: emergency-transition narratives persist rhetorically even as permanent institutional interests become visible. This trajectory is consistent with mandatrophy: the founding problem has been solved or deferred to indefiniteness, the constraint persists anyway due to military institutional interests, and the legitimation narrative becomes theatrical. The constraint avoids mandatrophy resolution only if: (1) the founding problem is genuinely live and the threat landscape remains high enough to justify emergency veto authority, or (2) the constraint is openly reclassified from transitional emergency measure to permanent institutional structure requiring fresh democratic legitimation. The current state (indefinite emergency, rising performativity) is neither: it holds the worst features of both mandatrophy (expired founding problem, extractive persistence) and institutional opacity (no fresh legitimation sought or offered). An omega variable documents this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_status_divergence,
    'Is the founding problem (state instability during post-revolutionary transition) still live, or has it been solved and the military constraint persists as institutional rent-seeking?',
    'Counterfactual analysis: if military veto were removed tomorrow, would the state face genuine collapse/civil conflict (live problem) or would governance reorganize around civilian democratic institutions (solved problem)? Longitudinal assessment: do threat levels and state-capacity metrics support continued emergency authority, or do they show improvement that should trigger constraint relaxation?',
    'If problem is solved, the constraint is mandatrophic and should be reclassified from tangled_rope (coordination + extraction) to snare (pure extraction with thin legitimation narrative). This would trigger institutional reform or constitutional amendment. If problem remains live, the high extraction is the price of security and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_divergence, empirical, 'Whether the constraint''s founding emergency rationale remains valid or has become obsolete cover for institutional extraction.').

omega_variable(
    charter_text_ambiguity_reading_selection,
    'What does the July Charter text actually say about military roles and civilian-military balance, and why does the military custodian reading win over the secular democratic reading in official interpretation?',
    'Textual analysis and interpretive genealogy: trace how the Charter''s language evolved from drafting through ratification, identify which clauses are ambiguous vs. determinate, document the process by which military custodian reading became canonized as official interpretation (was it drafted that way, or imposed through institutional power after ratification?), examine whether secular democratic reading is textually viable.',
    'If the Charter''s text actually mandates civil-military balance and the military reading is a power-driven reinterpretation, the constraint shifts from constitutional structure to interpretive usurpation (a form of snare that uses text as cover). If the text is genuinely ambiguous and the military reading is one legitimate interpretation among several, the constraint becomes a reading-contest (multiple constraints from one kernel) rather than a constitutional violation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_text_ambiguity_reading_selection, conceptual, 'Whether the military custodian reading is mandated by Charter language or imposed through institutional power; whether alternative readings are textually viable.').

omega_variable(
    identity_locking_mechanism_student_movement,
    'Are students and civil society activists remaining suppressed because of structural barriers (arrest, surveillance, violence) or because political voice has become identity-fused with nationalist/religious framing that the military controls?',
    'Longitudinal interviews with detained activists post-release, analysis of rhetoric shift among suppressed populations, examination of whether activist networks reorganize under nationalist/religious framing that aligns with military authority, test whether suppression persists after structural barriers are formally removed (comparative analysis with jurisdictions that ended military veto).',
    'If suppression is purely structural, removing military enforcement machinery would restore autonomous mobilization within weeks. If suppression is partially internalized (political voice has been fused with nationalist/military identity frames), removing enforcement would not fully restore autonomous voice — activists would carry the suppression forward. This affects both the sustainability of constraint removal and the classification of suppression mechanism (structural vs. internalized).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locking_mechanism_student_movement, empirical, 'Whether student/civil society suppression is structural enforcement or partially internalized identity fusion with military-controlled nationalism.').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Does the military custodian reading logically foreclose the secular democratic reading within the same framework, or do they coexist as competing interpretations held by different institutional factions?',
    'Analyze whether military and democratic advocates each claim to be interpreting the SAME Charter toward different outcomes, or whether they claim the Charter was drafted ambiguously and each is selecting one viable reading. If both claim to be reading the same text uniquely, foreclosure is in play; if each claims ambiguity permits their reading, coexistence applies.',
    'If foreclosure: the readings are logically incompatible and institutional resolution (winner-take-all) is mathematically necessary. If coexistence: both readings remain live positions held by different factions, and resolution is political (whichever faction holds power can impose its reading). This affects the terminal attractor of the constraint and whether reform is possible via amendment or only via factional victory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'Whether military custodian and secular democratic readings are logically exclusive or politically competitive.').

omega_variable(
    international_alliance_conditionality,
    'Would international allies withdraw security support if the military custodian reading were displaced and democratic civilian control were restored, or do they support military guardianship only for contingent geopolitical reasons that would persist under democratic governance?',
    'Analyze geopolitical interests of allied powers: are they committed to military governance per se, or to stability and anti-ideological-drift regardless of governance form? Examine cases where allied powers withdrew support after democratic transition (did they?) and cases where they supported regime change from military to democratic rule (did they?). Document ally statements about support conditionality.',
    'If allies are committed to military guardianship per se, democratic transition would trigger ally defection and security vulnerability — a structural barrier to constraint removal. If allies are committed to stability regardless of governance form, democratic transition would be feasible without ally defection. This affects both the cost of constraint removal and the likelihood of military resistance on stability grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_alliance_conditionality, empirical, 'Whether international support for military custodian constraint is structural or contingent on geopolitical alignment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(july_tr_t7, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 7, 0.29).
narrative_ontology:measurement(july_tr_t14, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 14, 0.33).
narrative_ontology:measurement(july_tr_t21, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 21, 0.37).
narrative_ontology:measurement(july_tr_t35, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 35, 0.39).
narrative_ontology:measurement(july_tr_t50, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(july_be_t7, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 7, 0.71).
narrative_ontology:measurement(july_be_t14, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 14, 0.75).
narrative_ontology:measurement(july_be_t21, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 21, 0.78).
narrative_ontology:measurement(july_be_t35, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement(july_be_t50, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(july_su_t7, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 7, 0.76).
narrative_ontology:measurement(july_su_t14, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 14, 0.8).
narrative_ontology:measurement(july_su_t21, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 21, 0.83).
narrative_ontology:measurement(july_su_t35, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 35, 0.86).
narrative_ontology:measurement(july_su_t50, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__military_custodian_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% The July Charter kernel generates three distinct constraints corresponding to three competing interpretive readings. This file instantiates the military custodian reading: Charter ratifies military as permanent institutional guardian ensuring stability. The sibling constraints are the secular democratic reading (military subordinate to democratic civil authority) and the guided nationalism reading (religious identity as sovereign legitimacy ground). All three read the same constitutional text and generate different beneficiary/victim structures, different ε values, and different classifications. They are linked as constraint family members via the military_custodian_reading's reading_relations and the equivalent relations in sibling files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
