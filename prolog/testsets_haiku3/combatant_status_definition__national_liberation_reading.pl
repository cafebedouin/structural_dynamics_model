% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: National Liberation Combatant Status (AP I Article 1(4))
 *   domain: legal/humanitarian
 *
 * SUMMARY:
 *   Article 1(4) of Protocol I to the Geneva Conventions extends combatant
 *   status to organized non-state armed groups fighting colonial occupation,
 *   foreign occupation, or racist regimes if they meet criteria: organized
 *   command structure, responsible leadership, fixed distinctive sign, and
 *   adherence to laws of war. This reading instantiates that extension as a
 *   binding obligation, recognizing liberation movements as legitimate actors
 *   with POW-protection claims. The reading is contested: occupying states
 *   and their allies reject the applicability of Article 1(4); humanitarian
 *   bodies and post-colonial states affirm it; functional-protection
 *   advocates decouple status from entitlements. This JSON instantiates ONLY
 *   the national liberation reading, with ε assessing the constraint from the
 *   reading's own perspective: the standing arrangement under contest is the
 *   occupying state's unilateral denial of combatant status to organized
 *   non-state groups, which the reading would obligate them to grant if
 *   Article 1(4) criteria are met. The reading extracts from occupying-state
 *   discretion and confers on liberation movements — a substantial structural
 *   imbalance from the occupier's seat, moderate benefit-shift from the
 *   liberation-movement seat.
 *
 * KEY AGENTS:
 *   - National liberation movements: organized non-state armed groups fighting to end colonial or occupation regimes; gain conditional POW status if meeting Article 1(4) criteria
 *   - Occupying state military: institutional actor forced to recognize combatant status for qualifying opponents; loses detention discretion
 *   - Occupying state government: bears the political cost of legitimizing the opposition; constrained exit to negotiation rather than criminalization
 *   - International humanitarian-law bodies (ICRC, treaty committees): agenda-setter interpreting Article 1(4) and adjudicating group qualification
 *   - State-centric reading advocates: excluded by this reading's core premise; their categorical exclusion of non-state combatants is foreclosed
 *   - Captured fighters: powerless beneficiaries gaining POW protections if their movement qualifies
 *   - Civilian populations: dual-position beneficiaries of discrimination rules, payers if conflict prolongation increases casualty risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.62).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.71).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "National Liberation Combatant Status (AP I Article 1(4))").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "legal/humanitarian").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '96d787e1-a8da-4f26-ae97-b52f855efbdf').
narrative_ontology:cs_kernel_codification('96d787e1-a8da-4f26-ae97-b52f855efbdf', formalized).
narrative_ontology:cs_authority_grounding('96d787e1-a8da-4f26-ae97-b52f855efbdf', lineage).
narrative_ontology:cs_interpretation_layer_present('96d787e1-a8da-4f26-ae97-b52f855efbdf').
narrative_ontology:cs_reading_relation('96d787e1-a8da-4f26-ae97-b52f855efbdf', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('96d787e1-a8da-4f26-ae97-b52f855efbdf', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('96d787e1-a8da-4f26-ae97-b52f855efbdf', foundational, non_state_combatant_status_possible).
narrative_ontology:cs_axiom_status(non_state_combatant_status_possible, holdable).
narrative_ontology:cs_axiom_grounding('96d787e1-a8da-4f26-ae97-b52f855efbdf', non_state_combatant_status_possible, deontological).
narrative_ontology:cs_axiom('96d787e1-a8da-4f26-ae97-b52f855efbdf', foundational, self_determination_right_precedes_sovereignty).
narrative_ontology:cs_axiom_status(self_determination_right_precedes_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('96d787e1-a8da-4f26-ae97-b52f855efbdf', self_determination_right_precedes_sovereignty, deontological).
narrative_ontology:cs_reference_frame('96d787e1-a8da-4f26-ae97-b52f855efbdf', transparent_status_determination_regime).
narrative_ontology:cs_drift_state('96d787e1-a8da-4f26-ae97-b52f855efbdf', contemporary_occupation_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96d787e1-a8da-4f26-ae97-b52f855efbdf', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_state_military).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_state_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, captured_fighters).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, civilian_populations).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-state armed groups fighting to end colonial occupation or racist regimes gain combatant status — and thus POW protections if captured — if they satisfy AP I Article 1(4) criteria: organized command structure, responsible leadership, and adherence to laws of war. This reading extends them parity with state military actors in the status hierarchy. They remain trapped in the conflict (exit is political resolution, not personal choice) and operate at national scope bounded by the territory they contest.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, trapped, national).

% Bears the obligation under this reading to recognize qualifying liberation movements as combatants, granting POW status to captured fighters. This reduces their unilateral discretion in detention policy and exposes captured soldiers to retaliation under POW protections. The reading constrains military operations by requiring discrimination between combatants and protected persons, with verification burden on the occupying force.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_state_military, payer,
    institutional, generational, constrained, national).

% Must concede legitimacy to the opponent's status claim if Article 1(4) criteria are met. This delegitimizes a key framing advantage (insurgents as terrorists, not combatants) and creates pressure to negotiate rather than suppress. The state retains only constrained exit: it can contest whether specific groups meet criteria or deny the applicability of AP I, but cannot categorically exclude organized liberation movements from combatant status consideration once the reading is binding.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_state_government, payer,
    institutional, generational, constrained, national).

% The International Committee of the Red Cross and treaty bodies interpret and enforce this reading through advisory opinions, case assessment, and norm-setting. They mediate the conflict between state sovereignty claims and liberation movement recognition, deciding which groups meet criteria and how the reading applies. Their authority is grounded in lineage (Protocol I as formalized kernel) and extraction (institutional interest in stabilizing the combatant status regime).
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_humanitarian_law_bodies, agenda_setter,
    institutional, civilizational, analytical, universal).

% States that reject the national liberation reading — particularly occupying powers — would argue for the state-centric reading (non-state actors categorically excluded). They are structurally excluded from this story's framework: the reading itself forecloses their core claim by extending combatant status to qualifying non-state groups. Their voices persist in state practice and treaty reservations but are not recognized as legitimate within the AP I Article 1(4) framework.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, state_centric_reading_advocates, excluded,
    institutional, generational, constrained, universal).

% Individual fighters from qualifying movements gain POW status protections — humane treatment, medical care, fair trial rights, repatriation upon conflict end — rather than criminal or terrorist detention. Their benefit is contingent on organizational status and captured at a moment when exit is impossible. They are structurally unable to verify or contest whether their movement meets criteria; that determination is made by occupying-state adversaries and IHL bodies over their heads.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, captured_fighters, beneficiary,
    powerless, biographical, trapped, local).

% Benefit from the combatant-status rule's discrimination requirement: occupying forces must distinguish combatants from civilians, reducing indiscriminate violence. They also bear indirect costs where recognition of liberation movements as legitimate combatants prolongs conflict and increases civilian casualty risk. Exit is geographic or temporal (flee or wait out conflict); neither is practical in many occupation contexts.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, civilian_populations, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, civilian_populations, payer).

% Humanitarian actors and some IHL scholars argue that combatant status determinations should be decoupled from protection entitlements: all detained persons should receive Common Article 3 minimums (humane treatment, medical care, trial fairness) regardless of status. They observe this reading from outside its direct framework — they do not contest the status-determination criteria but propose a parallel protection floor that renders status-hierarchy distinctions secondary. Their reading coexists with this one in ongoing doctrine.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, functional_protection_reading_advocates, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, international_humanitarian_law_bodies).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified status-classification regime for non-state armed actors: instead of each state making unilateral determinations of who qualifies for combatant protections, the rule coordinates on transparent criteria (Article 1(4): organized, commanded, responsible, IHL-compliant). This enables predictable treatment and reduces incentives for occupying forces to invent ad-hoc status categories designed to deny protections.
% TRANSFER_FUNCTION: Transfers legitimate status and legal parity from occupying-state military exclusively to national liberation movements that meet AP I Article 1(4) criteria. Movements gain combatant recognition and POW protections; occupying states lose unilateral discretion to classify fighters as terrorists or criminals. The movement is status transfer — from illegitimacy (insurgent/criminal) to legitimacy (combatant) — not a flow of material goods.
% ABSENT_VOICES: Occupying states that reject the national liberation framing are structurally excluded — they do not participate in this reading's framework as equals; they are the targets of its obligation-generation. States that deny AP I applicability or contest whether specific movements meet Article 1(4) criteria are present in litigation and negotiation but lack voice in the IHL bodies' interpretation process if those bodies adopt the reading. Non-state armed groups that do NOT meet Article 1(4) criteria (failure on command-control, organization, or IHL adherence) are excluded: the reading offers no benefit to unorganized or undisciplined groups.
% DISAPPEARANCE_RATIONALE: If this reading — the extension of combatant status to AP I Article 1(4) groups — disappeared, the occupying state would revert to unilateral combatant determination (or denial). Captured fighters would lose POW protections en masse, reverting to criminal or terrorist detention at state discretion. The liberation movement would lose a legitimacy anchor and face greater pressure to adopt terrorist tactics (which the reading incentivizes against). International humanitarian-law bodies would lose a coordinating rule and resort to case-by-case assessment. The status hierarchy would reorganize around pure state power rather than transparent criteria.
% FOUNDING_PROBLEM: Early protocols to the Geneva Conventions addressed inter-state wars and formally organized state militaries. By the mid-20th century, colonial and racial occupations fought by organized but non-state groups created a status gap: occupying powers denied combatant status to deny POW protections; liberation movements lacked legal anchor for protection claims; humanitarian law had no binding rule for non-state actor recognition. AP I Article 1(4) (1977) was designed to close this gap by setting objective criteria rather than leaving status to occupier discretion.
% FOUNDING_PROBLEM_CORROBORATION: Occupying states and their allies argue the founding problem was overstated — non-state actors are inherently less accountable and POW status for insurgents creates perverse incentives. Humanitarian organizations, most post-colonial states, and liberation-movement advocates attest the problem remains live: occupying powers routinely deny combatant status to AP I Article 1(4)-qualifying groups and apply criminal law to deny protections. Neither corroborating source is independent of the value framework, but practice evidence from occupation contexts (Palestinian territories, Kashmir, Myanmar) shows denial of status by occupiers despite Article 1(4) satisfaction, indicating the founding coordination problem persists.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 endpoint) reflects the reading's substantial shift in discretionary power: occupying states lose the ability to unilaterally deny combatant status to organized groups meeting transparent criteria. This is not zero-sum extraction in the classical sense (one party's gain is another's loss), but a status-hierarchy reordering that imposes obligation on occupiers and bestows recognition on movements. Suppression is elevated (0.71) because the reading's persistence depends on active enforcement against occupying-state resistance: states systematically contest Article 1(4) applicability, refuse ICRC-advised status determinations, and apply criminal law despite IHL bodies' recognition. Theater is moderate (0.42) because IHL bodies maintain performative neutrality (objective criteria, due process) while the underlying dispute is power-driven: occupiers deny status to deny protections; IHL bodies maintain the criteria framework even when enforcement fails. The time-series trajectory shows rising theater and suppression from 1977 to 2026: as occupations persist and movements increasingly meet Article 1(4) criteria, IHL bodies must work harder (rising suppression_requirement) to maintain the rule against state resistance, while performative framing (theater) rises as states conduct quasi-judicial status reviews they plan to reject beforehand (rising theater from 0.25 to 0.42). Accessibility collapse (0.68) reflects that once the reading's applicability is understood, occupying states face only constrained alternatives: deny AP I applicability, contest group qualification, or negotiate — genuine exit (unilateral status denial) is foreclosed by the reading's rule.
 *
 * PERSPECTIVAL GAP:
 *   The occupying-state-military and occupying-state-government seats compute divergently from the national-liberation-movement seat. From the occupier's position, this reading is a constraint on sovereign military operations and detention policy — extraction of discretionary authority, suppression of unilateral classification power. The constraint is enforced against their active resistance and requires active suppression to maintain (occupiers invest in counter-narratives, alternative legal frameworks, denial of criteria satisfaction). From the liberation-movement seat, this reading is a coordination function — it provides a transparent, universally applicable status standard that anchors legitimacy claims and offers legal protection. The movement is beneficiary and coordinated party simultaneously. From the captured-fighter seat (powerless), the reading is contingent benefit: protection flows only if the movement qualifies and the occupier complies, both uncertain. From the IHL-body seat (observer), the reading is a status-stabilization mechanism requiring active maintenance against state defection. The engine computes these per-seat differences from the structural data (power, exit_options, beneficiary/victim declarations); the claim/metric independence rule ensures the authored metrics (0.62 extraction, 0.71 suppression) report what is descriptively true of the constraint's operation across all seats, not a reconciled average.
 *
 * DIRECTIONALITY LOGIC:
 *   Occupying-state actors (military, government) are structural targets (high d, near 1.0): they bear the constraint's extraction (loss of status discretion), face active suppression (must defend against the rule), and have constrained exit (cannot categorically reject the reading without abandoning AP I legitimacy). Their directionality toward this constraint is high extraction — they are the primary seats from which the rule extracts authority. National liberation movements are structural beneficiaries (low d, near 0.0): they gain status recognition, reduced vulnerability to criminalization, and legal parity-claims. Their exit is trapped (political resolution, not exit) but the constraint benefits them regardless. Captured fighters are similarly beneficiary-positioned but with higher vulnerability (powerless, biographical horizon, trapped exit). IHL bodies are near-symmetric (d ≈ 0.5): they coordinate the rule's maintenance but also extract institutional authority from being its arbiter. The reading_relations and axioms in cs_structure document how this directionality divergence arises from a contested kernel: different parties (occupiers vs. liberationists) read the same AP I text (combatant status rules) through different foundational axioms (sovereign military prerogative vs. self-determination rights), producing opposing directional effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (gap in status determination for non-state armed actors) remains contested (status=contested in six_questions). The disappearance_verdict is world_rearranges, meaning the reading's absence would substantially reorganize detention, protection, and legitimacy. This indicates the founding coordination problem is NOT resolved by the reading — the reading *claims* to solve it by setting objective criteria, but occupying states routinely deny applicability, contested groups cannot verify qualification, and captured fighters remain vulnerable. The constraint exhibits mandatrophy characteristics: its founding problem persists unsolved, its operation is increasingly theatrical (status reviews that pre-determine rejection), and enforcement suppression rises over time (states invest more in denial narratives). However, mandatrophy is not RESOLVED here — the constraint is not yet a pure zombie (it still determines protections in some jurisdictions and movements). This is a PRE-MANDATROPHY state: the founding problem is dying but the constraint persists by institutional inertia and IHL-body maintenance. A true mandatrophy_resolved declaration would require the founding problem to be demonstrably dead (status=dead) while the constraint persists, which is not yet the case — occupying powers still contest applicability rather than acceding to it. The reading would benefit from temporal extension and measurement through a conflict where a movement clearly meets Article 1(4) criteria and the occupying state clearly denies status despite meeting them — that mismatch-data is the mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_14_applicability_contest,
    'Does AP I Article 1(4) apply to all colonial and occupation contexts, or only those meeting additional legitimacy criteria (anti-racist struggle, genuine self-determination claim, non-terrorist methods)?',
    'Adjudicative reading by International Court of Justice, ICRC interpretation, or treaty conference protocols. Current practice shows divergence: South Africa''s apartheid struggle (clear AP I 1(4) fit) vs. contested movements (PKK, FARC) where occupier rejects both occupation premise and self-determination claim.',
    'If applicability expands to all organized groups fighting any occupation, more movements qualify for combatant status. If it narrows to ''legitimate'' struggles, occupying states gain discretion to deny status by contesting the legitimacy of their own occupation — the reading becomes a facade. If scope contracts further (only formally colonized territories), many contemporary occupations escape Article 1(4) entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_14_applicability_contest, conceptual, 'Whether Article 1(4) applicability is self-executing (meets criteria → status granted) or requires external legitimacy judgment.').

omega_variable(
    command_control_verification_ambiguity,
    'Who verifies whether a non-state armed group possesses organized command structure meeting Article 1(4) requirements? The occupying state (conflicted interest)? IHL bodies (limited field access)? The movement itself (self-serving)?',
    'Comparative case analysis: instances where independent verification occurred (neutral mediators, third-party humanitarian monitors) vs. cases of state unilateral determination. Track correlation between verification source and status determination.',
    'If verification is occupier-controlled, the reading''s criteria become pretexts for status denial (the reading transforms into a snare — formal criteria mask substantive discretion). If verification is IHL-body-controlled, occupiers may lack leverage to deny, elevating the reading''s actual enforcement. If verification is decentralized, status becomes contested and protections inconsistent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(command_control_verification_ambiguity, empirical, 'Whether Article 1(4) criteria verification is procedurally independent or institutionally captured.').

omega_variable(
    ihl_body_institutional_capture,
    'Do IHL bodies (ICRC, state parties) systematically favor occupying-state interpretations of Article 1(4) applicability over liberation-movement or humanitarian interpretations, particularly when powerful states are occupiers?',
    'Systematic audit of ICRC opinions, treaty-committee recommendations, and state-party votes on Article 1(4) qualification disputes. Track correlation between occupying-state military power and favorable determination rulings.',
    'If IHL bodies are captured by occupier interests, the reading''s enforcement mechanism is corrupted — the rule persists formally while status determinations systematically deny qualifying groups. This is the piton transition point: the rule becomes performance while the coordinate problem (status determination for non-state actors) remains unsolved. If bodies remain independent, the rule''s enforcement depends on occupier compliance, which remains low but at least unambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ihl_body_institutional_capture, empirical, 'Whether IHL interpretive bodies maintain independence from occupying-state institutional capture.').

omega_variable(
    reading_kernel_foreclosure,
    'Does the national_liberation_reading''s assertion that organized non-state groups CAN gain combatant status logically foreclose the state_centric_reading''s assertion that they CANNOT, or do the two readings describe different legal regimes (national law vs. international law, different treaty parties'' interpretations)?',
    'Doctrinal analysis: can a single legal actor (one state, one jurisdiction) simultaneously hold both readings as consistent? If yes, they coexist (different sub-framework interpretations). If no single actor can hold both, the readings foreclose each other within a unified framework.',
    'If foreclosure is real, adopting this reading commits the actor to rejecting the state-centric reading — status must be available to Article 1(4)-qualifying groups. If coexistence is possible (via sub-framework splitting), occupying states can accept the reading for some groups while rejecting it for others — the reading''s constraint dissolves into case-by-case discretion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether the national_liberation and state_centric readings are logically incompatible (foreclosure) or doctrinally separable (coexistence).').

omega_variable(
    suppression_internalization_mechanism,
    'Is the suppression measured (0.71) primarily structural (legal barriers, state enforcement machinery) or internalized (captured movements accept occupier status denial, internalize criminality narratives, abandon protection claims)?',
    'Post-occupation narrative analysis: do movements that achieve independence immediately reassert combatant-status claims and demand retrospective POW recognition, or do they accept the occupation-era status denials as legitimate? Interview-based study of captured-fighter reintegration: do post-release combatants report sustained belief in their criminalness, or rapid reframing upon exit?',
    'If suppression is primarily structural, the constraint''s force decays upon occupation end — status denial persists as institutional artifact, not internalized belief. If internalized, even post-occupation movements struggle to reassert status claims, and the suppression''s effects outlast the occupation itself. High internalization indicates the constraint operates deeper than legal rules, shaping combatant identity and legitimacy self-perception.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression of combatant-status claims is maintained by external legal/enforcement barriers or by captured groups'' internalized belief in their own illegitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement_basis(comb_tr_t1977, projected).
narrative_ontology:measurement(comb_tr_t1985, combatant_status_definition__national_liberation_reading, theater_ratio, 1985, 0.29).
narrative_ontology:measurement_basis(comb_tr_t1985, observed).
narrative_ontology:measurement(comb_tr_t1995, combatant_status_definition__national_liberation_reading, theater_ratio, 1995, 0.34).
narrative_ontology:measurement_basis(comb_tr_t1995, observed).
narrative_ontology:measurement(comb_tr_t2005, combatant_status_definition__national_liberation_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement_basis(comb_tr_t2005, observed).
narrative_ontology:measurement(comb_tr_t2015, combatant_status_definition__national_liberation_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(comb_tr_t2015, observed).
narrative_ontology:measurement(comb_tr_t2026, combatant_status_definition__national_liberation_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(comb_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.48).
narrative_ontology:measurement_basis(comb_be_t1977, projected).
narrative_ontology:measurement(comb_be_t1985, combatant_status_definition__national_liberation_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement_basis(comb_be_t1985, observed).
narrative_ontology:measurement(comb_be_t1995, combatant_status_definition__national_liberation_reading, base_extractiveness, 1995, 0.56).
narrative_ontology:measurement_basis(comb_be_t1995, observed).
narrative_ontology:measurement(comb_be_t2005, combatant_status_definition__national_liberation_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement_basis(comb_be_t2005, observed).
narrative_ontology:measurement(comb_be_t2015, combatant_status_definition__national_liberation_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(comb_be_t2015, observed).
narrative_ontology:measurement(comb_be_t2026, combatant_status_definition__national_liberation_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(comb_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.62).
narrative_ontology:measurement_basis(comb_su_t1977, projected).
narrative_ontology:measurement(comb_su_t1985, combatant_status_definition__national_liberation_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement_basis(comb_su_t1985, observed).
narrative_ontology:measurement(comb_su_t1995, combatant_status_definition__national_liberation_reading, suppression_requirement, 1995, 0.67).
narrative_ontology:measurement_basis(comb_su_t1995, observed).
narrative_ontology:measurement(comb_su_t2005, combatant_status_definition__national_liberation_reading, suppression_requirement, 2005, 0.69).
narrative_ontology:measurement_basis(comb_su_t2005, observed).
narrative_ontology:measurement(comb_su_t2015, combatant_status_definition__national_liberation_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement_basis(comb_su_t2015, observed).
narrative_ontology:measurement(comb_su_t2026, combatant_status_definition__national_liberation_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(comb_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__national_liberation_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, prisoner_of_war_protections__minimum_standards).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, terrorist_designation_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested combatant_status_definition kernel. Sibling readings (state_centric_reading, functional_protection_reading) are generated as separate constraint stories with different ε values, beneficiary/victim structures, and foundational axioms. The three readings together model how a single formal text (AP I Article 1(4)) instantiates different constraints depending on interpretive commitment (which party reads it, from what legitimacy frame). All three are linked via network.affects_constraints for contamination-propagation analysis: if one reading's authority erodes (e.g., state_centric reading loses state support), others' relative position shifts; if the functional_protection_reading gains adoption, it creates downstream pressure on both liberation and state readings by decoupling status from protections. This is a constraint family under the ε-invariance principle (OQ-26): each reading has its own standing arrangement referent, its own beneficiary structure, and its own ε assessment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
