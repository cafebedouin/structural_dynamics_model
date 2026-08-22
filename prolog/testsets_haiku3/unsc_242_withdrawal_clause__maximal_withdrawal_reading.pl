% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Maximal Withdrawal Clause — Full Territorial Retrocession
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) established a principle of territorial
 *   integrity following the Six-Day War. The constraint analyzed here is the
 *   maximal reading of the withdrawal clause: that the definite French
 *   article ('les territoires') mandates complete withdrawal from all
 *   occupied territories, binding the occupier to full retrocession. This
 *   reading contests the partial reading, which interprets the English
 *   indefinite article ('from territories') as permitting discretionary
 *   retention of strategic territories. The maximal reading instantiates a
 *   binding international law commitment that military occupation confers no
 *   legal title. The occupier bears a mandatory obligation to withdraw; the
 *   dispossessed claimant gains an enforceable legal position. The constraint
 *   is read as Rope (coordination on the norm that conquest is illegitimate)
 *   but functions with substantial extractive asymmetry (the occupier loses
 *   territory and strategic depth; the claimant regains sovereignty and
 *   territorial integrity). The claim-metric gap is deliberate: the reading
 *   is authored as Rope (genuine coordination on an international law
 *   principle); the metrics describe high extraction (0.82 at endpoint)
 *   because the occupier experiences the constraint as a net cost of
 *   compliance, not as coordination benefit.
 *
 * KEY AGENTS:
 *   - dispossessed_claimant_states: beneficiaries of the maximal reading, hold legal claims but lack enforcement capacity (regional power level, trapped exit)
 *   - occupying_military_power: payer, bears the withdrawal obligation, can reinterpret or resist enforcement (institutional power, constrained exit via reinterpretation lever)
 *   - de_facto_settler_administrations: payers with identity-locked exit, embedded in occupied territory (organized power, biographical horizon)
 *   - international_law_community: vindicated-proposition beneficiaries, interpreters and enforcers of the territorial integrity doctrine (institutional power, analytical seat)
 *   - security_council_permanent_members: agenda-setters controlling UNSC 242 interpretation (institutional power, mobile exit)
 *   - rival_occupying_powers: excluded from the interpretive authority, but bound by the same maximal reading in their situations (powerful, global scope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.82).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.71).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Maximal Withdrawal Clause — Full Territorial Retrocession").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '235f087e-5da4-4b0d-b933-6730b77b9828').
narrative_ontology:cs_kernel_codification('235f087e-5da4-4b0d-b933-6730b77b9828', fixed_text).
narrative_ontology:cs_authority_grounding('235f087e-5da4-4b0d-b933-6730b77b9828', extraction).
narrative_ontology:cs_interpretation_layer_present('235f087e-5da4-4b0d-b933-6730b77b9828').
narrative_ontology:cs_reading_relation('235f087e-5da4-4b0d-b933-6730b77b9828', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('235f087e-5da4-4b0d-b933-6730b77b9828', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('235f087e-5da4-4b0d-b933-6730b77b9828', foundational, military_conquest_confers_no_legal_title).
narrative_ontology:cs_axiom_status(military_conquest_confers_no_legal_title, holdable).
narrative_ontology:cs_axiom_grounding('235f087e-5da4-4b0d-b933-6730b77b9828', military_conquest_confers_no_legal_title, deontological).
narrative_ontology:cs_axiom('235f087e-5da4-4b0d-b933-6730b77b9828', foundational, withdrawal_scope_is_comprehensive).
narrative_ontology:cs_axiom_status(withdrawal_scope_is_comprehensive, holdable).
narrative_ontology:cs_axiom_grounding('235f087e-5da4-4b0d-b933-6730b77b9828', withdrawal_scope_is_comprehensive, empirically_contingent).
narrative_ontology:cs_reference_frame('235f087e-5da4-4b0d-b933-6730b77b9828', charter_territorial_integrity_norm).
narrative_ontology:cs_drift_state('235f087e-5da4-4b0d-b933-6730b77b9828', contemporary_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('235f087e-5da4-4b0d-b933-6730b77b9828', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimant_states).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, territorial_integrity_doctrine_adherents).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_military_power).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, de_facto_settler_administrations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_law_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal claims to occupied territories under the maximal reading of UNSC 242. The constraint binds the occupier to full retrocession. They lack the military capacity to enforce unilaterally but gain an international law enforcement mechanism grounded in Charter Article 2(4) and the definite-article French text. Their enforcement is via ICJ, UN bodies, and diplomatic pressure. Exit from the constraint means abandoning the legal claim itself, which is politically infeasible.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimant_states, beneficiary,
    powerful, generational, trapped, regional).

% Bears the legal obligation to withdraw from all occupied territories under this reading. Military possession does not translate to legal title. The constraint's enforcement machinery (ICJ rulings, UNSC resolutions, diplomatic isolation) creates material costs to non-compliance. Withdrawal threatens strategic depth, settler-community interests, and regional positioning. The occupier's practical exit is limited: formal treaty withdrawal is costly and rare; reinterpretation toward the partial reading is the primary lever.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_military_power, payer,
    institutional, generational, constrained, regional).

% Are embedded administrative structures built on occupied territory under the occupier's authority. They have institutional identity fused with territorial control and cannot exit without territorial withdrawal. They bear the political cost of the constraint's operation directly — they lose territory and institutional standing if the maximal reading is enforced. Their resistance is organized through political lobbying of the occupier and denial of the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, de_facto_settler_administrations, payer,
    organized, biographical, identity_locked, local).

% Interprets and enforces the Charter Article 2(4) territorial integrity principle. Under the maximal reading, they vindicate the positivist doctrine that military conquest does not confer legal title. They benefit from constraint operation by solidifying the norm; they bear enforcement costs (diplomatic capital, institutional capacity). As institutional interpreters, they are analytically seated, not subject to the constraint's extraction.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_law_community, beneficiary,
    institutional, civilizational, analytical, global).

% Set and enforce UNSC 242 interpretation through voting, veto threat, and resolution drafting. They collectively control which reading of the withdrawal clause is operational. Their agenda-setting power is constrained by the Charter text and by diplomatic consensus-building costs, but they can shift interpretation through precedent and alliance. The maximal reading is the reading they officially endorse via successive resolutions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, security_council_permanent_members, agenda_setter,
    institutional, generational, mobile, global).

% Are other states that occupy territories they do not hold legal title to under international law. The maximal reading of 242 applies to their situations as well, binding them to withdrawal. If they were seated at the negotiating table, they would argue for the partial reading (discretionary withdrawal, security exceptions). Their exclusion from the constraint's interpretive authority is what permits the maximal reading to persist without their veto.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rival_occupying_powers, excluded,
    powerful, generational, trapped, global).

% Adjudicate the textual meaning of UNSC 242 and its binding force. They read the French definite article ('les') as mandatory scope-closure (all territories) versus the English indefinite article ('from territories') as discretionary scope. Their interpretive authority is contested by the occupier and by drafting-state intent readers. As observers, they measure the constraint but are not subject to its extraction.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj_and_treaty_interpreters, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_military_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding international legal norm that military occupation does not confer legal sovereignty or permanent territorial title. Coordinates states around the principle that territorial changes must be negotiated and consensual, not imposed by force. Solves the collective-action problem of preventing permanent territorial conquest by force, which would otherwise incentivize arms races and preemptive occupation.
% TRANSFER_FUNCTION: Transfers the legal presumption of legitimacy from the occupier (who holds territory militarily) to the dispossessed claimant (who holds the international law claim). Under the maximal reading, the occupier must transfer territory back; under the partial reading, the occupier retains discretion over transfer scope and conditions.
% ABSENT_VOICES: Rival occupying powers are excluded from the interpretive process — they are bound by the same maximal reading in their situations but not seated in its negotiation. The occupier itself contests the maximal reading through sponsoring alternative interpretations (partial reading, security-exception doctrine). De facto settler populations are administratively organized but politically subordinate to the occupying military power's veto over interpretation.
% DISAPPEARANCE_RATIONALE: If the maximal withdrawal constraint ceased to be operative, occupying powers would retain indefinite territorial control; territorial disputes would resolve through military strength rather than negotiated return. The claimant states would lose their primary enforcement mechanism and would face pressure to accept permanent partition. The international law regime structuring territorial disputes would collapse into a power-only framework.
% FOUNDING_PROBLEM: Post-1945 decolonization and territorial disputes: how to prevent permanent territorial conquest by force and ensure that occupied territories return to their original sovereigns or become independent. The founding problem arose from WWII territorial shifts, Cold War proxy conflicts, and decolonization-era occupation.
% FOUNDING_PROBLEM_CORROBORATION: The dispossessed claimant states and the international law community attest the founding problem is live — ongoing occupations persist and the mechanism remains necessary. The occupying power contests the status, arguing the security situation justifies retention. Diplomatic records and UN debates show the founding problem is actively invoked by both sides; the contest is real, not settled. Independent scholars of international law and the UN Secretary-General have issued reports attesting the founding problem's persistence; these sources are outside the immediate beneficiary-seat circle.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at 2024) because the constraint binds the occupier to comprehensive retrocession — no discretionary scope for retention. The obligation is mandatory and leaves no arbitrage escape. The occupier's only practical lever is reinterpretation toward the partial reading, which is costly diplomatically. Suppression is moderate-high (0.71) because the maximal reading must be defended actively: the occupier contests it, settler populations resist, and rival occupying powers have incentive to undermine it. Theater ratio is low-moderate (0.28) because the constraint operates through genuine legal mechanisms (ICJ, UNSC, diplomatic pressure) rather than ceremonial compliance, but a growing share of the occupier's response is rhetorical reinterpretation and denial rather than material withdrawal. The measurement series tracks the constraint over 57 years on a shared time grid: extractiveness rises modestly as the occupier's strategic position hardens (later timepoints show more entrenched occupation, making withdrawal more costly); theater ratio rises as rhetorical defense of non-withdrawal increases relative to actual withdrawal actions; suppression requirement rises as the occupier must invest more in diplomatic and legal counterarguments to block enforcement. All three metrics rise together, consistent with a constraint whose functional operation becomes more effortful to maintain and more rhetorically elaborate to justify.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant state's seat: the maximal reading is the correct interpretation of the UN Charter, binding and enforceable, and represents the triumph of law over force. From the occupier's seat: the maximal reading is an overly broad construction of ambiguous text that ignores drafting-state intent (the English indefinite article), security exceptions, and practical necessity. From the settler administration's seat: the constraint is an external imposition that threatens their community's survival and territorial home, delivered by absent international judges who do not bear the security consequences of withdrawal. From the Security Council's seat: the maximal reading is the official interpretation they have endorsed, but they retain the power to shift it toward the partial reading if political pressure mounts. The engine computes these differences from the structural data — the authorized divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The dispossessed claimant states derive d near 0.0 (beneficiary end): the constraint creates their legal position and enforces their claim at no cost to them. The occupying power derives d near 1.0 (target end): the constraint extracts territory, strategic depth, and administrative control. The international law community derives d near 0.0 (it vindicates a doctrine it profits from maintaining without bearing extraction costs). Security Council permanent members are mobile (they set the reading and can shift it, so their directionality is intermediate and strategic, ~0.5). De facto settler populations derive high d because they are identity-locked to occupied territory and experience the constraint as existential threat. This divergence in directionality is the primary source of seat divergence: the beneficiary seats (claimants, law community) experience this as genuine coordination on a shared principle; the payer seats (occupier, settlers) experience it as coerced transfer of territorial control.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is territorial conquest prevention; the constraint's function is to bind occupiers to return territories. At the 1967 origin, the constraint operated at high fidelity — occupation was meant to be temporary, and the expectation was withdrawal once security was restored. By 2024, the constraint persists while occupation has hardened into semi-permanent settlement and administrative infrastructure. The founding problem's status is contested (the occupier claims security conditions still justify retention; the claimant states claim the founding problem is solved and the constraint has become a hostage to non-compliance). The constraint has not resolved its founding problem after 57 years; instead, it has become a site of ritualized non-compliance. The theater ratio rising from 0.12 to 0.28 reflects this dynamic: the occupier engages with the constraint through interpretive reframing (arguing for the partial reading, security exceptions, negotiated solutions) rather than withdrawal. This is the piton-warning pattern — the constraint persists via institutional inertia and rhetorical maintenance while its primary function (ensuring temporary occupation) has atrophied. The extraction calculation remains high because non-compliance itself is extractive — the occupier retains territory it should not under the maximal reading. The constraint has not resolved to piton because the beneficiary seats (the claimants, the law community) still actively maintain it through legal argument and diplomatic pressure; it is not yet a purely theatrical fossil. But the trajectory is toward mandatrophy — a constraint whose founding problem has been displaced by an entrenched counter-equilibrium (permanent occupation) that neither side can break.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_vs_english_article_semantic_equivalence,
    'Do the French definite article (''les territoires'') and the English indefinite article (''from territories'') have semantically equivalent scope, or does the French construction mandate comprehensive scope while the English permits discretion?',
    'Linguistic analysis by French-English legal translation experts; examination of other multilingual UN instruments for article-usage conventions; comparison to the drafting deliberations to determine if article choice was deliberate or accidental.',
    'If semantically equivalent, the text is genuinely ambiguous and both readings have equal textual grounding — the interpretation authority question (which seat interprets) becomes decisive. If French mandates while English permits, the maximal reading has stronger textual support, and the occupier''s reinterpretation lever weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(french_vs_english_article_semantic_equivalence, empirical, 'Whether the withdrawal clause is textually ambiguous or mandates maximum scope in one of its language versions.').

omega_variable(
    drafting_intent_vs_text_authority,
    'What level of authority should be granted to the drafting states'' original intent regarding discretionary withdrawal, versus the plain text as read by later interpreters?',
    'Jurisprudential debate within the ICJ and legal scholarship; precedent from prior cases on intent vs. text in treaty interpretation; evolution of the Vienna Convention on the Law of Treaties interpretation principles.',
    'If drafting intent is dispositive, the partial reading gains authority (the drafters did intend discretion). If the text alone governs, the maximal reading prevails. This is a preference-class question about interpretive methodology, not an empirical fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drafting_intent_vs_text_authority, preference, 'Jurisprudential dispute over whether to privilege drafting intent or later textual reading in contested treaty language.').

omega_variable(
    customary_international_law_evolution,
    'Has the customary international law norm evolved toward mandated withdrawal from all occupied territories, or has a counter-norm of security-justified retention hardened into customary law through repeated non-compliance?',
    'State practice analysis: track withdrawals vs. retentions since 1967 and their justifications. Opinio juris analysis: examine legal arguments states advance for their position. Compare the frequency and acceptability of withdrawal vs. retention as precedent.',
    'If mandated-withdrawal has become customary law, the maximal reading is strengthened to the point of binding force independent of UNSC interpretation. If retention has become customary-practice-through-repetition, the partial reading is vindicated as reflecting evolved state practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_international_law_evolution, empirical, 'Whether state practice has crystallized toward the maximal or partial reading as operative customary international law.').

omega_variable(
    constraint_reading_vs_negotiated_solution,
    'Is the maximal reading meant to be enforced as law, or is it a negotiating anchor point that both sides use rhetorically while accepting that the real outcome will be negotiated compromise?',
    'Examine the actual settlement outcomes in disputes subject to UNSC 242: do they track the maximal reading (full withdrawal), the partial reading (negotiated retention of strategic areas), or negotiated splits between them? Track the language used by both occupier and claimant in their rhetorical positioning.',
    'If enforcement as law is the norm, the constraint operates as stated. If the readings function as rhetorical anchors in negotiation, the constraint''s actual extractiveness is lower than the law-framed measurement suggests — it becomes a negotiating framework rather than a binding rule. This would lower ε and shift the type toward tangled_rope (coordination through negotiation anchored by conflicting legal readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_reading_vs_negotiated_solution, conceptual, 'Whether the maximal reading represents enforced international law or a negotiating framework where both readings are rhetorical tools.').

omega_variable(
    kernel_identity_under_contest,
    'Is the kernel of UNSC 242 the text itself, or is it the principle of ''land for peace'' that both the maximal and partial readings claim to instantiate?',
    'Examine UN discourse and state arguments: do they appeal to textual fidelity or to the principle? If the principle is the real kernel and the text is secondary, the readings can coexist as different implementations of the shared principle.',
    'If the principle is the real kernel, the readings coexist rather than foreclose — both serve ''land for peace,'' just with different withdrawal scope. If the text is the kernel, the readings compete for textual authority and the maximal reading forecloses the partial reading within a unified law-based framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_under_contest, conceptual, 'Whether UNSC 242''s kernel is a text or a principle, and whether the maximal and partial readings are competitors or compatible implementations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement_basis(unsc_tr_t1967, observed).
narrative_ontology:measurement(unsc_tr_t1978, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1978, 0.16).
narrative_ontology:measurement_basis(unsc_tr_t1978, observed).
narrative_ontology:measurement(unsc_tr_t1988, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1988, 0.2).
narrative_ontology:measurement_basis(unsc_tr_t1988, observed).
narrative_ontology:measurement(unsc_tr_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(unsc_tr_t2000, observed).
narrative_ontology:measurement(unsc_tr_t2012, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement_basis(unsc_tr_t2012, observed).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(unsc_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.68).
narrative_ontology:measurement_basis(unsc_be_t1967, observed).
narrative_ontology:measurement(unsc_be_t1978, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1978, 0.72).
narrative_ontology:measurement_basis(unsc_be_t1978, observed).
narrative_ontology:measurement(unsc_be_t1988, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1988, 0.75).
narrative_ontology:measurement_basis(unsc_be_t1988, observed).
narrative_ontology:measurement(unsc_be_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement_basis(unsc_be_t2000, observed).
narrative_ontology:measurement(unsc_be_t2012, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2012, 0.81).
narrative_ontology:measurement_basis(unsc_be_t2012, observed).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.82).
narrative_ontology:measurement_basis(unsc_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement_basis(unsc_su_t1967, observed).
narrative_ontology:measurement(unsc_su_t1978, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1978, 0.61).
narrative_ontology:measurement_basis(unsc_su_t1978, observed).
narrative_ontology:measurement(unsc_su_t1988, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1988, 0.64).
narrative_ontology:measurement_basis(unsc_su_t1988, observed).
narrative_ontology:measurement(unsc_su_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2000, 0.67).
narrative_ontology:measurement_basis(unsc_su_t2000, observed).
narrative_ontology:measurement(unsc_su_t2012, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2012, 0.69).
narrative_ontology:measurement_basis(unsc_su_t2012, observed).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(unsc_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% The UNSC 242 withdrawal clause is contested across three structurally distinct readings: maximal_withdrawal (this story), partial_withdrawal, and interpretive_authority_structure. Each reading has different ε, beneficiary/victim structure, and type. The maximal reading (this story) assumes the French definite article controls and mandates comprehensive withdrawal, binding the occupier to full retrocession. It is instantiated as Rope but with high measured extraction. The partial reading interprets the English indefinite article as discretionary, permitting security-justified retention. The authority-structure reading shifts the contest from textual meaning to the question of interpretive authority (ICJ vs. drafting states vs. customary practice). Each reading must be authored separately to preserve ε-invariance; they are linked by network.affects_constraints to show their structural relationship as competing framings of the same kernel. The engine measures their competition through cross-reading coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
