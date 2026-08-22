% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Popular Sovereignty and Electoral Legitimacy (Republican Reading)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The republican reading of sovereignty holds that legitimate authority
 *   flows upward from the people through delegated consent — the people hold
 *   ultimate authority, which they loan to representatives through elections,
 *   and retain the right to recall that authority. This reading emerged in
 *   tension with monarchical authority (divine right, inheritance) and hybrid
 *   constitutional frames (mixed authority sources). The constraint extracts
 *   from those excluded from franchise while coordinating the enfranchised
 *   citizenry through periodic elections. The extractiveness is moderate
 *   (0.38) because the reading is genuinely coordinative for those it
 *   includes, but suppression is substantial (0.52) because the boundary of
 *   'the people' must be actively maintained and those outside it face
 *   coercion without voice. Theater ratio (0.41) reflects the gap between the
 *   ideal of popular sovereignty (delegated, removable, accountable
 *   authority) and the practice (media influence, wealth-power concentration,
 *   gerrymandering, voter suppression). This is a KERNEL READING: one of
 *   three competing framings of the same legitimacy kernel, so the narrative
 *   focuses on THIS reading's structural claim, beneficiary/victim
 *   distribution, and validation mechanisms — sibling readings (monarchical,
 *   constitutional-hybrid) are OTHER constraints, not described here.
 *
 * KEY AGENTS:
 *   - enfranchised_citizenry: theoretical locus of ultimate authority under this reading; benefits from the claim that authority flows upward and that representatives are accountable
 *   - elected_representatives: hold delegated authority; benefit from legitimacy framing that shields them from force-based challenges; subject to electoral removal
 *   - disenfranchised_populations: governed without consent; bear costs of the boundary that defines 'the people' as enfranchised only; structural cost-bearers of the constraint
 *   - excluded_non_citizens: denied franchise; subject to law and authority they did not consent to; their exclusion is structural to the reading's definition of 'the people'
 *   - constitutional_enforcement_apparatus: maintains the boundary conditions of electoral legitimacy — ballot access, term limits, removal mechanisms; translates the claim into institutional structure
 *   - analytical_observer: measures the gap between the legitimacy claim and the actual distribution of voice and power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.38).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.52).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Popular Sovereignty and Electoral Legitimacy (Republican Reading)").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '92025621-5c61-4809-bf71-0228ac50718a').
narrative_ontology:cs_kernel_codification('92025621-5c61-4809-bf71-0228ac50718a', fixed_text).
narrative_ontology:cs_authority_grounding('92025621-5c61-4809-bf71-0228ac50718a', lineage).
narrative_ontology:cs_interpretation_layer_present('92025621-5c61-4809-bf71-0228ac50718a').
narrative_ontology:cs_reading_relation('92025621-5c61-4809-bf71-0228ac50718a', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('92025621-5c61-4809-bf71-0228ac50718a', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('92025621-5c61-4809-bf71-0228ac50718a', foundational, popular_sovereignty_foundational).
narrative_ontology:cs_axiom_status(popular_sovereignty_foundational, holdable).
narrative_ontology:cs_axiom_grounding('92025621-5c61-4809-bf71-0228ac50718a', popular_sovereignty_foundational, deontological).
narrative_ontology:cs_axiom('92025621-5c61-4809-bf71-0228ac50718a', foundational, electoral_removal_mechanism_essential).
narrative_ontology:cs_axiom_status(electoral_removal_mechanism_essential, holdable).
narrative_ontology:cs_axiom_grounding('92025621-5c61-4809-bf71-0228ac50718a', electoral_removal_mechanism_essential, instrumental).
narrative_ontology:cs_axiom('92025621-5c61-4809-bf71-0228ac50718a', secondary, inherited_authority_illegitimate).
narrative_ontology:cs_axiom_status(inherited_authority_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('92025621-5c61-4809-bf71-0228ac50718a', inherited_authority_illegitimate, deontological).
narrative_ontology:cs_reference_frame('92025621-5c61-4809-bf71-0228ac50718a', enlightenment_popular_consent).
narrative_ontology:cs_drift_state('92025621-5c61-4809-bf71-0228ac50718a', contemporary_wealth_concentration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('92025621-5c61-4809-bf71-0228ac50718a', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizenry).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_populations).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, excluded_non_citizens).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, popular_sovereignty).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, social_contract_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, delegation_of_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens with voting rights exercise delegated power to elect representatives and remove them through electoral cycles. They benefit from the legitimacy doctrine that grounds authority in their consent — this doctrine is the mechanism through which they retain theoretical authority over governance. They hold electoral leverage and constitutional voice. Their power is diffuse and periodic but structurally foundational to the reading's legitimacy claim.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizenry, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, enfranchised_citizenry, agenda_setter).

% Exercise delegated authority claimed to flow from popular sovereignty. They implement policy, make laws, and hold executive power between elections. Their authority is conditional on electoral validation and subject to removal, but they hold concentrated power during their mandate. They benefit from the legitimacy claim that they rule by the people's consent, which shields them from challenges based on inherited authority or force alone.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_representatives, agenda_setter,
    institutional, biographical, constrained, national).

% Governed by representatives they did not elect and cannot remove (historically: enslaved people, colonized populations, women, non-property-owners; contemporaneously: undocumented immigrants, minors, incarcerated people). They bear the full apparatus of governance without access to the consent mechanism. They are the cost-bearers of the republican legitimacy reading — the constraint extracts their compliance while denying them the mechanism through which legitimacy is supposed to be claimed.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_populations, payer,
    powerless, generational, trapped, national).

% Resident or present in the polity but denied citizenship and franchise. They are subject to law and authority but have no formal voice in the consent mechanism. Their exclusion is structurally required for the republican reading to function — the reading defines legitimacy through the consent of 'the people,' which requires a boundary that excludes non-citizens. This boundary concentrates power among enfranchised citizens at the cost of those deemed outside the people.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, excluded_non_citizens, payer,
    powerless, immediate, trapped, national).

% Courts, constitutional bodies, and legal systems that interpret and enforce the bounds of delegated authority and electoral processes. They maintain the structural conditions for popular sovereignty to function — ballot access, representation rules, term limits, removal mechanisms. They adjudicate conflicts between the people's claim and governmental action. Their power is constrained by the constitutional text but real in its reach.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Present in the polity and subject to its laws but denied formal franchise participation. They would argue that governance affecting them should require their input; their structural exclusion from the consent mechanism is the defining feature of the republican reading's extraction — authority over them is claimed without their voice, justified by the boundary of 'the people.'
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, non_voting_residents, excluded,
    powerless, biographical, trapped, national).

% Monarchical, theocratic, or autocratic systems that ground authority differently (inherited right, divine mandate, expertise). They are logically incompatible with the republican reading's claim that authority flows upward from popular consent. They are kept out of the framework not by force but by the reading's core premise — the two frames cannot coexist in one polity's legitimacy structure.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, rival_authority_systems, excluded,
    institutional, civilizational, trapped, global).

% Views the constraint from outside the partisan structure — examines the relationship between the legitimacy claim and the actual distribution of voice, the gap between the 'people' as theorized and as empirically constituted, the mechanisms of enforcement, and the vulnerabilities of the reading.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of authority justification: how can governance claim legitimacy without appeal to inherited divinity, force, or expertise alone? The republican reading answers: through the claim that authority is delegated by the people through electoral consent. This coordinates expectations — government acts, citizens accept the act as legitimate because they elected the actor and can remove them. It replaces force-based or tradition-based acceptance with consent-based acceptance.
% TRANSFER_FUNCTION: Moves voice and power: from the many (the people) to the few (elected representatives), justified by the claim that the few hold that power temporarily, delegated by the many, accountable to the many through elections. It also moves the capacity to set legitimacy boundaries: enfranchised citizens determine who counts as 'the people,' and thus who has standing to consent. This boundary-setting is transferred from explicit inherited status to a semi-open democratic franchise.
% ABSENT_VOICES: Disenfranchised populations and non-citizens excluded from the franchise are kept structurally out of the conversation about legitimacy. A colonized people might object that their lack of consent means the government ruling them is not legitimate, but the republican reading's boundary excludes them from that objection's force — they are not counted as part of 'the people' whose consent matters. Rival authority systems (monarchies, theocracies) are absent from the speaking order, excluded by the core premise of the reading itself.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty / electoral legitimacy claim vanished overnight, political authority would require new justification — would revert to inherited authority, force, expertise, or some hybrid. Governments would lose their primary shield against delegitimacy claims. Revolutionary pressure and political instability would surge. The absence of this constraint does not mean the world returns to nature; it means the justificatory frame for authority shifts, and governance reorganizes around a different legitimacy claim.
% FOUNDING_PROBLEM: Medieval and early-modern authority was grounded in hierarchical inheritance, divine sanction, and force. As economies grew complex, merchant classes demanded voice, and printing distributed political ideas, the inherited-authority frame cracked. The founding problem the republican reading was built to solve: how can authority be claimed as legitimate when inheritance and divinity no longer persuade? Answer: ground it in the consent of the governed, delegated through elections.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists and constitutional scholars from outside the benefiting parties (notably: those facing majoritarian tyranny, those excluded from franchise) attest that the founding problem — the legitimacy crisis of inherited authority — was real and the republican reading was a substantive innovation. However, they also contest whether the solution actually solved the problem: does electoral consent truly legitimate, or does the machinery of elections merely ritualize power that flows from wealth, media control, or state capacity? Historians and political scientists document both the real shift from inherited-authority frames to popular-sovereignty frames AND the persistence of excluded populations and majoritarian extraction even in ostensibly republican systems.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint is genuinely coordinative: enfranchised citizens do get periodic voice and removal mechanisms. Elections are real, not pure theater. But extractiveness is NOT low because the constraint's persistence depends on actively maintaining the boundary of 'the people' — disenfranchised populations are excluded, and that exclusion requires enforcement (voter ID laws, citizenship requirements, gerrymandering that dilutes voting power). Suppression (0.52) is high relative to extractiveness because the boundary must be maintained coercively; those outside it have no voice through which to challenge the arrangement. Theater ratio (0.41) reflects the rising gap between the ideal of popular sovereignty and the practice: media influence, campaign finance, gerrymandering, and voter suppression mean that while elections occur, the popular voice is increasingly filtered through wealth and organizational capacity. The measurement trajectory shows slow extraction creep over the first 15 time points (representing historical drift as franchise expands but wealth concentration increases), peak at midpoint (t=20, where theater and suppression both rise), then modest stabilization as franchise protections strengthen and extraction plateaus. The constraint is TANGLED ROPE, not ROPE, because it combines real coordination (for enfranchised) with asymmetric extraction (from excluded) and requires active enforcement (boundary maintenance, voter suppression resistance, electoral machinery). A ROPE would have beneficiaries and victims both participating in the coordination; a TANGLED ROPE has them in structural opposition.
 *
 * PERSPECTIVAL GAP:
 *   The enfranchised-citizen seat and the disenfranchised-population seat compute entirely differently. From the citizen seat (moderate power, mobile exit, included in consent mechanism), the constraint looks like genuine coordination: they have voice, they can remove representatives, authority is grounded in their consent. The engine should compute this seat as experiencing moderate coordination with low extraction (d near 0.3–0.4, beneficiary-end). From the disenfranchised seat (powerless, trapped exit, excluded from consent), the same constraint looks like pure coercion: they are governed without voice and have no mechanism to withdraw consent. The engine should compute this seat as experiencing high extraction (d near 0.8–0.9, target-end). The directionality derivation chain — beneficiary/victim declarations, power atoms, exit options — should produce this divergence automatically. The reading does NOT adjudicate which seat's experience is 'correct'; the point is that the SAME CONSTRAINT produces opposite classifications at different seats, and that divergence is diagnostic of the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Enfranchised citizenry are declared beneficiaries: they participate in the consent mechanism, their voice is theoretically foundational to legitimacy, they can remove representatives. Their power is 'organized' (collective action through voting), their exit is 'mobile' (they can move to other polities, switch parties, organize politically). These factors push d toward the beneficiary end (d ≈ 0.25–0.35 depending on scope and other modifiers). Disenfranchised populations are declared victims: they are governed without consent access, they have no removal mechanism, their exclusion is structurally required to define 'the people.' Their power is 'powerless,' their exit is 'trapped' (cannot leave the jurisdiction easily, cannot change their status within it). These factors push d toward the target end (d ≈ 0.75–0.85). The directionality derivation works from the structural data: beneficiaries with high power and mobile exit get low d (they benefit and can leave); victims with no power and trapped exit get high d (they bear costs and cannot escape). No override is needed; the derivation from the declared structure produces the right divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving function) is NOT applicable to this constraint. The founding problem — how to ground authority without inheritance or divinity — is NOT obsolete. The republican reading's mandate is to justify authority through popular consent and elections. While the practice diverges from the ideal (wealth concentration, voter suppression, media influence), the mandate itself remains live and contested. Disenfranchised populations do not argue the consent-based legitimacy frame is obsolete; they argue they should be included in it. The constraint does NOT persist because its original function has been forgotten; it persists because enfranchised populations benefit from the legitimacy claim it provides and disenfranchised populations have no mechanism to force its replacement. If mandatrophy were present, we would expect the founding problem to be marked 'dead' and disappearance_verdict to be 'world_unchanged' — neither is true. Instead, the founding problem is 'contested' and disappearance_verdict is 'world_rearranges' because the constraint is actively maintained and genuinely shapes how authority is justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_sovereignty_vs_elite_capture,
    'Does the republican reading''s claim that authority flows upward from popular consent remain empirically true when wealth, media access, and campaign finance concentrate the capacity to shape electoral outcomes?',
    'Comparative analysis of electoral outcomes against wealth distribution, media ownership, and campaign spending across multiple democracies and historical periods. If electoral results consistently align with wealth interests regardless of franchise expansion or voter preferences, the claim that authority flows from popular consent becomes formally ambiguous (capture thesis vs. convergent interests).',
    'If elite capture is pervasive, the constraint reclassifies from TANGLED_ROPE (genuine coordination plus asymmetric extraction) to SNARE (coordination is cover story; extraction is primary). The founding problem moves from ''live'' to ''partially dead'' — the problem of justified authority remains, but the republican solution no longer solves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_sovereignty_vs_elite_capture, empirical, 'Whether popular sovereignty survives elite wealth concentration or becomes a cover story.').

omega_variable(
    boundary_of_the_people_instability,
    'Is the boundary of ''the people'' (who counts as enfranchised) stable, or does the reading require ongoing contestation and expansion to maintain its legitimacy claim?',
    'Historical analysis: does the reading survive with a fixed franchise boundary, or does legitimacy pressure force expansion (women''s suffrage, racial integration, age-based inclusion)? If legitimacy derives from actual consent of the governed, what happens when substantial populations demand inclusion?',
    'If the boundary must expand to maintain legitimacy, the reading is unstable with respect to exclusion — it contains internal pressure to include disenfranchised populations, making the current extraction (from excluded populations) temporary. If the boundary can stabilize, the reading can justify permanent exclusion, making extraction durable. This determines whether mandatrophy creeps in over generational scales.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_of_the_people_instability, conceptual, 'Whether the republican reading''s boundary of ''the people'' is stable or under structural pressure to expand.').

omega_variable(
    delegation_vs_direct_democracy_gap,
    'Does delegating authority to representatives constitute genuine consent, or does it hollow out popular sovereignty by placing decision-making beyond direct popular control?',
    'Normative and pragmatic: communities experimenting with direct democratic mechanisms (ballot initiatives, citizen assemblies, participatory budgeting) report on whether direct involvement feels more like exercise of authority than electoral delegation. If direct mechanisms are demanded and adopted, the reading''s delegation model is insufficient.',
    'If delegation is insufficient, the reading must extend its concept of legitimate authority to include direct mechanisms, or it risks being displaced by more participatory competitors (participatory_democracy_constraint, if authored). This does not refute the republican reading but limits its scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_vs_direct_democracy_gap, conceptual, 'Whether representative delegation adequately instantiates popular sovereignty or degrades it.').

omega_variable(
    monarchical_kernel_contest,
    'Is the republican reading''s core premise (authority flows upward from popular consent) logically foreclosed by the monarchical reading''s core premise (authority flows downward from sovereign through inheritance), or do they coexist in the same polity through institutional separation?',
    'Examination of constitutional hybrid systems (UK, Netherlands, Spain, Belgium, etc.) where hereditary monarchy coexists with popular electoral authority. Can a single polity hold both premises, or does one logically eliminate the other? If coexistence is possible, the readings are competitors (coexist_with); if one logically rules out the other, they foreclose.',
    'If the readings foreclose each other (the choice is binary), the constraint''s persistence depends on the republican reading displacing the monarchical. If they coexist, the constraint''s scope is limited to the electoral-authority domain and does not claim total legitimacy. This affects how the engine ranks the constraint in legitimacy hierarchies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monarchical_kernel_contest, conceptual, 'Whether republican and monarchical legitimacy framings foreclose each other or coexist.').

omega_variable(
    extractiveness_from_excluded_populations,
    'What is the actual cost borne by disenfranchised populations and excluded non-citizens relative to the coordination benefit enfranchised citizens receive? Is the extraction temporary (pending franchise expansion) or durable (justified by non-citizenship or other permanent status)?',
    'Comparative historical analysis: does the republican reading resolve by expanding franchise (supporting a ''temporary extraction'' reading, where extraction is an interim cost), or do persistent populations remain excluded despite literacy, legal presence, and demonstrated capacity to participate? If exclusion persists, extraction is durable and the constraint cannot be reclassified as temporary by virtue of expansion.',
    'If extraction is temporary and franchise-expansion is the reading''s expected trajectory, extractiveness should decline over generational scales (measurements should show decline after t=20). If extraction is durable, extractiveness should stabilize or oscillate. This determines whether the constraint''s type changes as history progresses or whether it remains a stable TANGLED_ROPE.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_from_excluded_populations, empirical, 'Whether exclusion-based extraction is temporal or durable within the republican reading''s logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(sove_tr_t0, observed).
narrative_ontology:measurement(sove_tr_t5, sovereign_legitimacy__republican_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement_basis(sove_tr_t5, observed).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__republican_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(sove_tr_t10, observed).
narrative_ontology:measurement(sove_tr_t15, sovereign_legitimacy__republican_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(sove_tr_t15, observed).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__republican_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(sove_tr_t20, projected).
narrative_ontology:measurement(sove_tr_t25, sovereign_legitimacy__republican_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(sove_tr_t25, projected).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__republican_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(sove_tr_t30, projected).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__republican_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(sove_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(sove_be_t0, observed).
narrative_ontology:measurement(sove_be_t5, sovereign_legitimacy__republican_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement_basis(sove_be_t5, observed).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__republican_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(sove_be_t10, observed).
narrative_ontology:measurement(sove_be_t15, sovereign_legitimacy__republican_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(sove_be_t15, observed).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__republican_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement_basis(sove_be_t20, projected).
narrative_ontology:measurement(sove_be_t25, sovereign_legitimacy__republican_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement_basis(sove_be_t25, projected).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__republican_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(sove_be_t30, projected).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__republican_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(sove_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(sove_su_t0, observed).
narrative_ontology:measurement(sove_su_t5, sovereign_legitimacy__republican_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(sove_su_t5, observed).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__republican_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(sove_su_t10, observed).
narrative_ontology:measurement(sove_su_t15, sovereign_legitimacy__republican_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(sove_su_t15, observed).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__republican_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(sove_su_t20, projected).
narrative_ontology:measurement(sove_su_t25, sovereign_legitimacy__republican_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement_basis(sove_su_t25, projected).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__republican_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(sove_su_t30, projected).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__republican_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(sove_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__republican_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The sovereign_legitimacy kernel decomposes into three structurally distinct constraint stories, one per reading: republican_reading (upward delegation, this file), monarchical_reading (downward inheritance), and constitutional_hybrid_reading (dual authority sources). These are NOT three perspectives on one constraint — they have different ε values, different beneficiary/victim sets, different legitimacy validation mechanisms, and different persistence conditions. They are linked by network.affects_constraints because they compete for the same kernel: if one reading's premises are accepted, the others' premises are either foreclosed or displaced to secondary status. Each story must be authored independently as a clean ε-invariant constraint; the engine computes per-seat classifications from the structural data, and the network edges enable contamination analysis (if republican legitimacy erodes, what pressure does that place on the monarchical and hybrid readings?).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
