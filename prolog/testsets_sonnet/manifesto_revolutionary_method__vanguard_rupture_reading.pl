% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__vanguard_rupture_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Seizure of State Power / Dictatorship of the Proletariat (Transitional State Reading)
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This story instantiates the vanguard-rupture reading of the Manifesto's
 *   revolutionary-method kernel: the claim that emancipation requires an
 *   organized party to seize state power outright and administer a
 *   transitional 'dictatorship of the proletariat' until class antagonism is
 *   abolished and the state itself withers away. At founding, the arrangement
 *   is presented as an emergency coordination measure against
 *   counter-revolutionary violence. Over the modeled interval, base
 *   extractiveness and the required suppression of rival organs both climb
 *   substantially — the historical pattern (Bolshevik Russia, and comparable
 *   cases) in which the 'transitional' state consolidates rather than
 *   dissolving. This is a distinct constraint from the sibling readings
 *   (council-communist and democratic-gradualist), each of which has its own
 *   ε and its own file; they are not alternate measurements of this one
 *   arrangement but structurally different arrangements the same kernel text
 *   is read to authorize.
 *
 * KEY AGENTS:
 *   - party_central_committee: Primary agenda_setter (institutional/arbitrage) — seizes and administers transitional state power
 *   - party_cadres: Primary beneficiary (organized/constrained) — staff the apparatus, gain durable position
 *   - state_planning_apparatus: Institutional beneficiary/agenda_setter (institutional/arbitrage) — absorbs coordination functions previously distributed
 *   - political_pluralists, autonomous_worker_councils, rival_socialist_factions, peasant_smallholders: Primary payers/excluded (moderate-to-powerless/trapped-constrained) — bear suppression and extraction
 *   - constitutional_theorists: Analytical observer — assesses transition-vs-permanence across historical cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.79).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure of State Power / Dictatorship of the Proletariat (Transitional State Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, 'b2233909-4140-47c6-972e-c5aa4fd965ac').
narrative_ontology:cs_kernel_codification('b2233909-4140-47c6-972e-c5aa4fd965ac', fixed_text).
narrative_ontology:cs_authority_grounding('b2233909-4140-47c6-972e-c5aa4fd965ac', lineage).
narrative_ontology:cs_interpretation_layer_present('b2233909-4140-47c6-972e-c5aa4fd965ac').
narrative_ontology:cs_reading_relation('b2233909-4140-47c6-972e-c5aa4fd965ac', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_reading_relation('b2233909-4140-47c6-972e-c5aa4fd965ac', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('b2233909-4140-47c6-972e-c5aa4fd965ac', foundational, party_vanguard_necessary_for_state_seizure).
narrative_ontology:cs_axiom_status(party_vanguard_necessary_for_state_seizure, holdable).
narrative_ontology:cs_axiom_grounding('b2233909-4140-47c6-972e-c5aa4fd965ac', party_vanguard_necessary_for_state_seizure, instrumental).
narrative_ontology:cs_axiom('b2233909-4140-47c6-972e-c5aa4fd965ac', foundational, transitional_dictatorship_permissible_pending_class_abolition).
narrative_ontology:cs_axiom_status(transitional_dictatorship_permissible_pending_class_abolition, holdable).
narrative_ontology:cs_axiom_grounding('b2233909-4140-47c6-972e-c5aa4fd965ac', transitional_dictatorship_permissible_pending_class_abolition, empirically_contingent).
narrative_ontology:cs_reference_frame('b2233909-4140-47c6-972e-c5aa4fd965ac', vanguard_party_seizure_of_state_power).
narrative_ontology:cs_drift_state('b2233909-4140-47c6-972e-c5aa4fd965ac', post_1991_socialist_bloc_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b2233909-4140-47c6-972e-c5aa4fd965ac', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_central_committee).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_councils).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, rival_socialist_factions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, peasant_smallholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs the seizure of state apparatus, determines who counts as authentically revolutionary, and administers the transitional dictatorship in the name of the proletariat as a whole. Sets the tempo and boundaries of permissible political organization during the transition, and controls when (or whether) the state is judged ready to 'wither away.'
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_central_committee, agenda_setter,
    institutional, generational, arbitrage, national).

% Staff the party and state organs created by the seizure of power; gain positions of administrative authority, career advancement, and material security conditional on ideological discipline and loyalty to the central line. Their standing depends entirely on the party's continued monopoly on legitimate revolutionary authority.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres, beneficiary,
    organized, biographical, constrained, national).

% Administers centralized economic and social coordination on behalf of the party-state, absorbing functions previously distributed across markets, guilds, unions, and local assemblies. Its authority and resource claims grow in direct proportion to how much autonomous coordination it displaces.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, agenda_setter).

% Advocate for multi-party competition, independent press, and electoral contestation even under proletarian rule. Under this reading they are treated as objectively counter-revolutionary or as instruments of the old order, and their organizing is suppressed, banned, or absorbed into party-sanctioned fronts. They cannot exit the polity without abandoning political voice altogether.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    moderate, biographical, trapped, national).

% Formed spontaneously at workplaces and localities to exercise direct control over production and local governance. Under vanguard rupture, their independent decision-making is subordinated to party directives or dissolved into party-controlled trade union structures; refusal is treated as factionalism or sabotage.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_councils, payer,
    organized, biographical, constrained, regional).

% Hold alternative readings of revolutionary strategy (gradualist, council-communist, anarchist) and would contest the vanguard's monopoly on interpreting the proletariat's interests if given a platform. They are excluded from the transitional state's institutions, often criminalized as deviationist or liquidated in factional struggles internal to the movement.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, rival_socialist_factions, excluded,
    moderate, biographical, trapped, national).

% Own or work land outside the industrial proletariat the vanguard claims to represent. Collectivization or requisition policy imposed by the transitional state extracts surplus from them to fund industrialization and cadre administration, with little recourse against a state that controls both the market and the means of coercion.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, peasant_smallholders, payer,
    powerless, biographical, trapped, regional).

% Study whether the transitional dictatorship functions as a genuinely temporary instrument of class abolition or becomes a permanent apparatus that outlives its justificatory function. They compare historical cases across differing outcomes but hold no power to alter the arrangement themselves.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the seizure and defense of state power against a counter-revolutionary bourgeoisie and foreign intervention during a period when the old order's coercive apparatus is not yet dismantled and no alternative organ has consolidated sufficient force to hold the revolution's gains.
% TRANSFER_FUNCTION: Moves formal political authority, coercive capacity, and control over production from the deposed ruling class and from competing organs of working-class self-organization (councils, rival parties, independent unions) into the hands of the party apparatus, which administers them nominally on behalf of the proletariat as a class.
% ABSENT_VOICES: Autonomous worker councils, rival socialist factions, and political pluralists would each argue for retaining or building parallel/plural sources of authority rather than ceding them to a single party; under this reading they are structurally excluded from the transitional state's decision-making, often through outright suppression rather than persuasion.
% DISAPPEARANCE_RATIONALE: If the party's exclusive claim on the transitional state dissolved, authority would immediately fragment toward whichever organs held de facto coercive or organizational capacity at that moment — councils, rival parties, or a reconstituted bourgeois state — and the administrative apparatus built around party monopoly would have to either compete for legitimacy or collapse.
% FOUNDING_PROBLEM: A revolutionary rupture leaves state power momentarily uncaptured; without a disciplined organization able to seize, hold, and wield that power against counter-revolution, the old ruling class (or an equally hostile new one) reconstitutes itself and the revolution is reversed.
% FOUNDING_PROBLEM_CORROBORATION: Party historians and surviving cadres attest the founding problem remains live wherever counter-revolutionary threat persists. Independent historians of twentieth-century revolutions, council-communist theorists, and dissident factions from within the same revolutionary movements attest that in every sustained historical instance the 'transitional' apparatus outlived any plausible counter-revolutionary threat and became a permanent structure of party rule — this corroboration comes from outside the benefiting party apparatus and from former insiders who broke with it.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and suppression (0.79) are both authored high and rising because the arrangement's persistence depends on actively foreclosing rival organs of working-class power (councils, competing parties) rather than out-competing them through voluntary coordination. Theater ratio rises moderately (0.10 → 0.32) as party structures increasingly perform proletarian representation (congresses, elections within party-sanctioned lists) while substantive decision authority concentrates in the central committee. Accessibility collapse (0.62) reflects that once the party monopolizes coercive capacity, alternative paths (council federation, multi-party contestation) become progressively harder to reconstruct. Resistance (0.58) is substantial but asymmetric — organized worker councils and rival factions resist, but their organizational capacity is exactly what the constraint's enforcement machinery targets first.
 *
 * DIRECTIONALITY LOGIC:
 *   Party central committee and state planning apparatus sit at the beneficiary end: they set terms, administer resources, and their institutional survival depends on the arrangement continuing. Party cadres are near-beneficiaries with a dependency clause — their gains are conditional on continued loyalty, giving them constrained rather than arbitrage exit. Political pluralists, worker councils, rival factions, and peasant smallholders sit at the target end: each bears suppression or extraction through the same structure that claims to coordinate the revolution on their behalf, and their exit options range from constrained to fully trapped because leaving the polity or renouncing political voice is the only alternative to compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncaptured state power reverting to counter-revolutionary control) is genuinely live in the earliest phase of any such seizure — this is why the story does not claim a false coordination story from t=0. But the temporal measurements model the mandatrophy pattern directly: as the counter-revolutionary threat recedes, suppression and extraction keep climbing rather than falling, which is the signature of a transitional justification persisting past its founding function. The six-questions R5 fields register this explicitly — status is 'contested' precisely because the party's own account (problem still live) is corroborated only from inside the beneficiary set, while outside corroboration (dissident factions, independent historians) attests the problem is functionally dead and the apparatus has become self-perpetuating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_vs_permanent_state_form,
    'Is the dictatorship of the proletariat, as administered by a vanguard party, structurally capable of self-dissolution once class antagonism recedes, or does the concentration of coercive and administrative capacity it requires make self-dissolution improbable regardless of the founders'' intent?',
    'Comparative historical analysis of every sustained instance of vanguard-party rule: track whether party-state apparatus contracted, remained stable, or expanded as external counter-revolutionary threat diminished.',
    'If self-dissolution is structurally improbable, the transitional justification is a permanent cover story and the constraint should be read closer to snare than tangled_rope; if genuine cases of contraction exist, the tangled_rope reading (real coordination function alongside extraction) holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_vs_permanent_state_form, empirical, 'Whether the transitional state form is structurally capable of withering away as claimed.').

omega_variable(
    vanguard_rupture_reading_identity,
    'This constraint is one reading (vanguard_rupture_reading) of the manifesto_revolutionary_method kernel. The sibling readings — council_communist_reading and democratic_gradualism_reading — locate the same founding text''s authority in different organs (federated worker councils vs. existing democratic institutions) rather than in a centralized party-state. Where exactly does the disagreement sit?',
    'The disagreement is located in the reading of ''proletarian dictatorship'' as an institutional form: this reading treats it as necessarily requiring a centralized party organ wielding state coercion; the council-communist reading treats the same phrase as compatible only with federated council power with no separate party-state; the gradualist reading treats revolutionary transformation as achievable without any rupture in existing state form at all. No shared textual exegesis resolves this without also resolving a prior empirical/strategic claim about whether gradual or council-based power can survive counter-revolutionary violence.',
    'If the council-communist reading is adopted instead, the beneficiary set shifts from party cadres/state apparatus to federated worker assemblies, and the primary victim set changes from pluralists/councils to any party apparatus attempting to impose itself over council authority — a structurally different constraint, not a different measurement of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vanguard_rupture_reading_identity, conceptual, 'Committer-structure omega: identifies this constraint as one reading among three siblings and locates the exact point of disagreement.').

omega_variable(
    coalition_capacity_of_suppressed_seats,
    'Could autonomous worker councils, rival socialist factions, and peasant smallholders — each individually weaker than the party apparatus — form an effective coalition capable of checking party monopoly, or does the party''s control of coercive capacity foreclose coalition formation before it can consolidate?',
    'Examine historical instances where such coalitions formed early (e.g., early soviet pluralism before consolidation) versus where they were suppressed before forming; compare timing of party consolidation relative to coalition formation windows.',
    'If coalition formation is foreclosed early and systematically, this strengthens the case that suppression (not mere organizational immaturity) explains the absence of resistance in the historical record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_capacity_of_suppressed_seats, empirical, 'Whether powerless/moderate victim groups could have coordinated resistance absent early suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mani_tr_t8, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(mani_tr_t16, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(mani_tr_t24, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(mani_tr_t32, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mani_be_t8, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(mani_be_t16, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(mani_be_t24, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(mani_be_t32, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mani_su_t8, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(mani_su_t16, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(mani_su_t24, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(mani_su_t32, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint, manifesto_revolutionary_method__council_communist_reading, and manifesto_revolutionary_method__democratic_gradualism_reading form a three-member reading-family over the shared manifesto_revolutionary_method kernel. Each reading instantiates a structurally distinct constraint with its own beneficiary/victim structure and its own ε: this reading (vanguard_rupture) carries the highest ε (0.68) due to its dependence on active suppression of rival organs; the council-communist reading is expected to carry the lowest ε among the three (coordination without a separate coercive party-state); the democratic-gradualism reading occupies an intermediate position (works through existing pluralist institutions rather than rupture). All three should be linked bidirectionally once authored.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
