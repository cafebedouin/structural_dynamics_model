% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Doctrine: Nuclear-Constrained Winnability as Maintained Planning Space
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   Since 1945, the strategic community has maintained a working answer to
 *   the question nuclear weapons posed to military organization: force
 *   retains utility if victory space can be narrowed rather than abolished.
 *   This story authors that maintenance as a constraint — the
 *   countervailing_thinkable arrangement in which limited victory remains a
 *   reachable, plannable object, sustained by targeting cycles, doctrine
 *   production, wargaming, and the procurement streams that convert 'hold
 *   forces at risk' into hardware. The arrangement solves a real problem
 *   (deterrence without executable options is brittle; institutions need
 *   failure contingencies) while extracting asymmetrically: it diverts
 *   budgets toward counterforce-specific capabilities, corrodes the
 *   bargaining position of arms-control institutions, normalizes elevated
 *   nuclear-use probabilities for populations under targeting plans, and
 *   rewards exactly the constituencies whose careers and revenues depend on
 *   the planning space staying open. KEY AGENTS (by structural relationship):
 *   - strategic_warfighting_establishment: Agenda setter
 *   (institutional/identity_locked) — runs targeting cycles, doctrine, and
 *   wargames; exit would dissolve its professional function -
 *   military_industrial_complex: Primary beneficiary (powerful/constrained) —
 *   converts winnable-war requirements into program lines and collects the
 *   resulting revenue - extended_deterrence_allies: Secondary beneficiary
 *   with embedded costs (institutional/constrained) — receives executable
 *   assurance while hosting the target set - arms_control_institutions:
 *   Primary victim among institutions (organized/trapped) — negotiates
 *   against a constraint that continuously removes its bargaining chips -
 *   civilian_populations_of_targeted_states: Diffuse victims
 *   (powerless/trapped) — absorb normalized use-risk with no seat anywhere in
 *   the process - defense_budget_taxpayers: Payer seat (moderate/constrained)
 *   — funds the recurring modernization waves the requirement generates -
 *   minimum_deterrence_advocates: Excluded voice (moderate/analytical) —
 *   argues the alternative posture from outside the allocation rooms -
 *   national_command_authority: Dual-positioned executive
 *   (powerful/constrained) — approves the doctrine and inherits the menus it
 *   produces
 *
 * KEY AGENTS:
 *   - strategic_warfighting_establishment: Agenda setter (institutional/identity_locked) — runs targeting cycles, doctrine, and wargames; exit would dissolve its professional function
 *   - military_industrial_complex: Primary beneficiary (powerful/constrained) — converts winnable-war requirements into program lines and collects the resulting revenue
 *   - extended_deterrence_allies: Secondary beneficiary with embedded costs (institutional/constrained) — receives executable assurance while hosting the target set
 *   - arms_control_institutions: Institutional victims (organized/trapped) — bargain against a structure that removes their leverage faster than treaties restore it
 *   - civilian_populations_of_targeted_states: Diffuse victims (powerless/trapped) — absorb normalized use-probabilities with no seat in any planning process
 *   - defense_budget_taxpayers: Payer (moderate/constrained) — fund recurring modernization justified by the winnable-war requirement
 *   - minimum_deterrence_advocates: Excluded (moderate/analytical) — articulate the minimum-deterrent alternative outside the allocation process
 *   - national_command_authority: Dual-positioned agenda setter and beneficiary (powerful/constrained) — approves doctrine and inherits the option menus it produces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.65).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.55).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Doctrine: Nuclear-Constrained Winnability as Maintained Planning Space").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '2767c1f3-eb8b-4a81-a8fc-a79a963ecd50').
narrative_ontology:cs_kernel_codification('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', formalized).
narrative_ontology:cs_authority_grounding('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', expertise).
narrative_ontology:cs_interpretation_layer_present('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50').
narrative_ontology:cs_reading_relation('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', war_winnability_post_1945__deterrence_unthinkable, forecloses).
narrative_ontology:cs_reading_relation('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', foundational, limited_victory_operationally_reachable).
narrative_ontology:cs_axiom_status(limited_victory_operationally_reachable, holdable).
narrative_ontology:cs_axiom_grounding('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', limited_victory_operationally_reachable, empirically_contingent).
narrative_ontology:cs_axiom('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', secondary, credible_deterrence_requires_executable_options).
narrative_ontology:cs_axiom_status(credible_deterrence_requires_executable_options, holdable).
narrative_ontology:cs_axiom_grounding('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', credible_deterrence_requires_executable_options, instrumental).
narrative_ontology:cs_reference_frame('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', limited_war_operational_continuity).
narrative_ontology:cs_drift_state('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', contemporary_second_nuclear_age, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2767c1f3-eb8b-4a81-a8fc-a79a963ecd50', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_warfighting_establishment).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_allies).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_institutions).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations_of_targeted_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, defense_budget_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, national_command_authority).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_allies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the annual targeting cycle, writes joint nuclear doctrine, staffs the wargames, and briefs the executive on available options. Produces and defends the graded-strike menus that make limited war a thinkable object. Its professional identity is constituted by translating political objectives into executable operations under nuclear constraint; accepting that no plan can bound escalation would dissolve the core function around which its institutions, career ladders, and analytical traditions are built.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_warfighting_establishment, agenda_setter,
    institutional, biographical, identity_locked, global).

% Receives multi-decade procurement streams justified by the standing requirement to hold adversary forces at risk: accurate submarine-launched missiles, stealth bombers, penetrating ISR constellations, hardened command-and-control, and now hypersonic and expanded-triad programs. Each winnable-war scenario converts into system requirements, program lines, and sustainment contracts. Product lines can shift within defense markets, but certification barriers, export controls, and customer concentration keep the industry inside the defense economy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    powerful, generational, constrained, continental).

% Hosts forward-deployed dual-capable aircraft, missile defenses, and in some cases nuclear weapons, and receives assurance that the protector's commitments are executable in graduated steps rather than apocalyptic bluff. Pays by proximity: hosting raises these territories' priority in adversary counterforce planning, shortens warning time, and makes them early nodes in any exchange the doctrine envisions.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_allies, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_allies, payer).

% Custodians of the treaty architecture — negotiation delegations, verification bureaucracies, review-conference machinery. Every deployed counterforce system is a bargaining chip removed from their ledgers and every warfighting posture signals that limitation is not the intent. They negotiate against the constraint but possess no lever to remove it; their staffing and mandate contract as warfighting postures expand.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_institutions, payer,
    organized, generational, trapped, global).

% Live under targeting plans that increasingly emphasize limited, counterforce strikes against military and leadership targets near population centers. Absorb the raised probabilities of nuclear use that limited-options doctrine normalizes, plus the tail risk that limited strikes fail to stay limited. Have no vote, no exit, and no seat in any planning process that allocates this risk to them.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations_of_targeted_states, payer,
    powerless, generational, trapped, global).

% Fund the recurring modernization waves the winnable-war requirement generates, across election cycles and rival administrations. Receive the argument that each wave purchases safety, while the arms-control-led alternative posture would redirect the same funds; individual preference exits through voting are heavily filtered by security framing and concentrated-interest advocacy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, defense_budget_taxpayers, payer,
    moderate, biographical, constrained, national).

% Scholars, former officials, and scientists in the arms-control and abolitionist traditions who argue that small, secure, retaliatory arsenals suffice and that counterforce competition is destabilizing and unnecessary. Publish, testify, and litigate at the margins — structurally outside the targeting cycle, budget hearings, and wargame schedule where the actual resources are allocated.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, minimum_deterrence_advocates, excluded,
    moderate, generational, analytical, global).

% Approves the doctrine and owns the decision if any plan is ever executed. Receives a menu of graded options shaped entirely by the planning establishment — gaining the appearance of calibrated control while inheriting assumptions, damage expectations, and escalation models it did not choose. The historical record shows executives demanding limited options (feeding countervailing development) and later chafing at the narrowness of the menus produced.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, national_command_authority, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, national_command_authority, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides escalation-management capacity that keeps deterrence credible: gives political leaders responses between capitulation and all-out exchange, assures allies that extended-deterrence commitments are executable rather than rhetorical, and maintains a continuous professional capability to translate policy objectives into bounded military operations under nuclear constraint.
% TRANSFER_FUNCTION: Moves budgetary resources from general treasuries into counterforce-specific capabilities (accuracy, penetration, ISR, survivable C2); moves strategic talent and institutional prestige into warfighting specialization; shifts existential risk onto the civilian populations of adversary states and allied host nations by legitimizing limited nuclear-employment scenarios; and drains negotiating leverage and institutional resources away from arms-control tracks.
% ABSENT_VOICES: Minimum-deterrence theorists, abolitionist scientists, and any representative of the targeted civilian populations are outside the planning conversation entirely. Arms-control custodians attend episodically — review conferences, treaty negotiations — but hold no veto over force posture, targeting doctrine, or procurement, which is where the constraint is actually maintained.
% DISAPPEARANCE_RATIONALE: If winnable-war planning ceased overnight — if the consensus became that no bounded victory space exists and planning stopped — counterforce procurement rationales would collapse, alliance assurance structures built on executable guarantees would require renegotiation, arms-control tracks would regain bargaining material and institutional weight, and the warfighting professions built around limited-options planning would lose their organizing mission. The strategic landscape would reorganize around deterrence-pure and disarmament-oriented postures.
% FOUNDING_PROBLEM: After 1945, military force lost its assumed utility: the dominant weapon could not be employed without catastrophic escalation, yet alliances, contingency obligations, and force structures still required operable plans. The founding problem was restoring graduated usability — answering 'what do we do if deterrence fails?' with plans that promise bounded rather than annihilating outcomes, so that deterrence remains credible and military institutions retain a mission.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: declassified directives (NSDM-242, PD-59) show civilian executives — not the services — demanding limited options after concluding all-or-nothing plans were unusable; independent strategic-studies scholarship on both sides of the winnability dispute treats bounded-escalation planning as a real response to a real problem rather than dismissing the problem itself; and successive administrations across opposed parties have retained the planning requirement, which is difficult to attribute solely to the benefiting parties' self-dealing.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.65 (current state): the constraint reliably converts a doctrinal premise into procurement streams decoupled from verified threat levels, strips negotiating leverage from arms-control tracks, and normalizes use-probabilities borne by people with no voice. Suppression is authored at 0.55 and is structural, not internalized: minimum-deterrence alternatives are not invisible, they are institutionally gated — excluded from targeting processes, budget hearings, and wargame schedules, with career consequences for insiders who press them. Theater ratio 0.33 reflects a real planning core wrapped in a performative layer: many wargames are structured to validate the option sets that justify the programs, and precision-era claims of surgical limitation outrun what the scenarios demonstrate. Accessibility_collapse 0.45: alternatives (arms-control-led posture, minimum deterrence, abolition) remain discoverable and are periodically attempted (test-ban initiatives, the freeze movement, post-Cold War reductions, the Prague agenda), but collapse under each crisis-driven return to rivalry. Resistance 0.60: the constraint meets sustained, organized opposition from the arms-control community, scientific societies, and mass movements across the interval.
 *   
 *   The temporal series show a crisis-driven cycle, not monotonic drift: extraction and enforcement rise with rivalry phases (1945–1985), relax during the unipolar interlude (1990s, when enforcement capacity visibly decayed and theater ratio peaked as planning drifted toward inertia), and rebuild with renewed multipolar rivalry (2000s–present). The oscillation is driven by external geopolitical phases rather than being itself an intermittent-reinforcement mechanism. All base_properties values reflect the t=80 endpoint — the re-risen phase of the cycle — which is the honest current state. The claim/metrics gap is deliberate and load-bearing: the constraint is CLAIMED as tangled_rope on structural grounds (a genuine coordination function entangled with asymmetric extraction under active enforcement), while the metrics describe its actual operation independently; the engine computes per-seat types from the structural data and measures any divergence.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical structure. From the planning establishment's seat, this is craft: a hard technical problem (bounding escalation) solved with increasing sophistication, deserving institutional permanence. From the arms-control seat, the same activity is sabotage: every accuracy improvement is a treaty chip destroyed and a signal that limitation was never intended. From the taxpayer seat it reads as expensive insurance; from the targeted-population seat it is a death probability assigned without consent; from the ally seat it is protection carrying a proximity surcharge; from the executive seat it is a menu that flatters control while foreclosing choices. The engine computes these divergences from the power/exit/role data; nothing in the authored claim adjudicates which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map directly: the planning establishment (low d — the constraint subsidizes its mission) and the military-industrial complex (lowest d — it collects the revenue the requirement generates) anchor the subsidy end; the establishment's identity_locked exit pulls it further toward structural dependence on the constraint persisting. Victims anchor the target end: arms-control institutions (high d, trapped — their entire mandate erodes under the constraint), targeted civilians (maximal d, powerless, no exit whatsoever), and taxpayers (high d, exit blocked by the structure of public finance). Extended-deterrence allies sit mid-range: genuine beneficiaries of executable assurance who simultaneously pay a proximity cost, captured by the secondary payer role rather than by an override. Suppression is treated as a raw structural property and is NOT scaled by power or scope in the engine's computation; only extractiveness scales — with global scope amplifying effective extraction because verification of restraint is hardest at planetary scale. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate every seat, and the override surface (keyed by power atom) is too coarse to improve on the derivation for the institution-level seats, which share a power atom but diverge correctly via their declared roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — restoring graduated usability to military force under nuclear constraint — remains live: deterrence-failure contingencies, alliance assurance obligations, and multipolar proliferation all still demand some answer to 'what if deterrence fails.' The arrangement is therefore not mandatrophy-resolved, and the flag is deliberately not set. Classification as tangled_rope prevents mislabeling in both directions: reading the structure as pure snare would erase the genuine coordination function (any nuclear state requires failure-contingency planning; allies rationally prefer executable guarantees to bluff), while reading it as pure rope would hide the asymmetric extraction (procurement capture, regime corrosion, risk imposition on voiceless populations) that the same structure delivers. The lifecycle risk runs toward piton rather than snare: if arms-control primacy were credibly restored, the planning apparatus could persist theatrically — wargames validating obsolete option sets, procurement justified by inertia — which the theater_ratio series (peaking at 0.42 during the function-starved 1990s) previews. The receipt-surface facts point the other way for now: gains concentrate in a named seat and fixing is prohibitive, which keeps the extraction side of the entanglement live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Which reading of the war_winnability_post_1945 kernel correctly locates the post-1945 winnability boundary — this reading (space non-empty and maintained), deterrence_unthinkable (space categorically empty), or rhetorical_contraction (space maintained but unsayable)?',
    'Cross-story comparison of the three sibling constraint files: each reading''s epsilon, beneficiary structure, and enforcement surface are authored independently; convergence or divergence in computed classifications against shared historical evidence (declassified planning documents, crisis behavior, procurement patterns) adjudicates which reading the structural data supports.',
    'Adopting deterrence_unthinkable would relocate this constraint''s beneficiaries from ''mission continuity holders'' to ''cover-story maintainers'' and drive epsilon sharply upward toward snare territory; adopting rhetorical_contraction would split the enforcement surface between operational and discursive registers and change which suppression mechanisms register.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'This constraint is one reading of a contested kernel; classification is conditional on reading adoption.').

omega_variable(
    escalation_control_empirical_basis,
    'Is bounded escalation actually controllable under nuclear constraint — the empirical foundation this reading''s foundational axiom rests on?',
    'Red-team wargame series with genuinely adversarial escalation incentives on both sides, plus systematic coding of declassified crisis records (Berlin, Cuba, 1973 alert, Indo-Pakistani crises) for whether graduated steps held; comparison against the entanglement and conventional-counterforce-crisis-instability literature.',
    'If bounded escalation proves uncontrollable, the coordination function this story declares collapses into cover, epsilon re-reads as nearly pure rent, and the classification migrates toward the deterrence_unthinkable sibling; if controllable in restricted conditions, the tangled_rope structure stabilizes with the caveat that applicability narrows to dyadic, decoupled scenarios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_control_empirical_basis, empirical, 'Empirical status of the escalation-control premise underlying winnable-limited-war planning.').

omega_variable(
    counterforce_rent_vs_necessity,
    'How much of the winnable-war planning enterprise''s persistence reflects genuine operational necessity versus military-industrial mission continuity and procurement capture?',
    'Compare planning intensity and counterforce investment against threat environments across administrations: the post-Cold War decade (threat collapsed, counterforce planning continued) and the modernization waves of the 2010s (threat framed as resurgent, investment accelerated) offer natural variation; program-line survival independent of stated threat levels indicates rent.',
    'A predominantly rent-driven finding pushes effective extraction up for the taxpayer and arms-control seats and supports snare-leaning recomputation; a predominantly necessity-driven finding validates the coordination half of the tangled_rope claim and holds the current classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_rent_vs_necessity, empirical, 'Depth of beneficiary capture versus operational necessity in the constraint''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0, 0.15).
narrative_ontology:measurement(war__tr_t10, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 10, 0.2).
narrative_ontology:measurement(war__tr_t20, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 20, 0.28).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 30, 0.32).
narrative_ontology:measurement(war__tr_t40, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 40, 0.38).
narrative_ontology:measurement(war__tr_t50, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 50, 0.42).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 60, 0.38).
narrative_ontology:measurement(war__tr_t70, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 70, 0.34).
narrative_ontology:measurement(war__tr_t80, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 80, 0.33).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(war__be_t10, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(war__be_t20, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(war__be_t40, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(war__be_t50, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(war__be_t70, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 70, 0.58).
narrative_ontology:measurement(war__be_t80, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 80, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(war__su_t10, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(war__su_t20, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(war__su_t40, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(war__su_t50, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(war__su_t60, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(war__su_t70, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 70, 0.48).
narrative_ontology:measurement(war__su_t80, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 80, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% The colloquial question 'are nuclear wars winnable?' decomposes into three structurally distinct constraint stories sharing the war_winnability_post_1945 kernel. This story (countervailing_thinkable) authors epsilon for the standing arrangement in which winnability space is held open and actively maintained by planning institutions. The sibling deterrence_unthinkable authors the arrangement in which winnability space is categorically empty and victory planning is incoherent. The sibling rhetorical_contraction authors the arrangement in which winnability remains operationally planned but discursively tabooed. The three carry different epsilon values, different beneficiary/victim structures, and different enforcement surfaces; this reading sits upstream of the rhetorical_contraction debate, since open countervailing doctrine is precisely what contraction-theorists document being rhetorically managed around, and stands in direct premise-negation with deterrence_unthinkable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
