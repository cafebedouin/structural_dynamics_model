% ============================================================================
% CONSTRAINT STORY: progressive_era_amendments__seventeenth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_progressive_era_amendments__seventeenth_amendment, []).

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
 *   constraint_id: progressive_era_amendments__seventeenth_amendment
 *   human_readable: Seventeenth Amendment: Senate Elections from Legislatures to People
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Seventeenth Amendment (ratified 1913) transferred U.S. Senate
 *   elections from state legislatures to direct popular vote. This constraint
 *   embodies the Progressive Era's diagnosis of a core structural problem:
 *   state legislatures had become mechanisms for selling Senate seats to the
 *   highest bidder. Corporations funded legislative candidates who would vote
 *   for pro-business senators; wealthy individuals bought influence through
 *   legislative intermediaries; party machines extracted rent from the
 *   mediation itself. The amendment's logic is reform through transparency —
 *   replacing a hidden mediation mechanism with direct popular
 *   accountability. However, the amendment instantiates a contested reading
 *   of federalism and democratic representation. The structural delta is
 *   stark: suppression of legislature-mediated selection; beneficiary becomes
 *   the statewide electorate; victims are state party machines and the
 *   federalism structure that legislature-mediated selection anchored;
 *   extractiveness of the purchased Senate seat is purportedly abolished.
 *   This constraint is one reading of the progressive_era_amendments kernel,
 *   and it coexists with sibling readings that made parallel claims about the
 *   Sixteenth Amendment (income tax), Eighteenth Amendment (prohibition), and
 *   Nineteenth Amendment (women's suffrage). Each reading targets a different
 *   extraction mechanism and claims a different mechanism for its abolition.
 *
 * KEY AGENTS:
 *   - Statewide Electorates: Primary beneficiary (powerless/trapped → mobile after amendment) — gain voice in Senate selection; shift from zero input to decisive electoral power
 *   - Progressive Reform Coalition: Secondary beneficiary (organized/constrained) — muckrakers, labor reformers, anti-corruption advocates pushing the amendment; benefit from reduced state machine power and increased federal receptiveness to reform agenda
 *   - State Party Machines: Primary victim (powerful/mobile) — lose the rent-extraction mechanism of legislative mediation; forced to adapt to direct electoral dynamics
 *   - Legislative Federalism Structure: Secondary victim (institutional/arbitrage) — the formal mechanism of state-mediated representation is hollowed out; states retain authority but lose institutional power over Senate selection
 *   - Incumbent Senators: Tertiary victim (powerful/mobile) — those elected through legislative channels face increased uncertainty; must now build personal constituencies rather than maintaining legislative relationships
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing federalism as immutable when the amendment redefines its mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(progressive_era_amendments__seventeenth_amendment, 0.38).
domain_priors:suppression_score(progressive_era_amendments__seventeenth_amendment, 0.62).
domain_priors:theater_ratio(progressive_era_amendments__seventeenth_amendment, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(progressive_era_amendments__seventeenth_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(progressive_era_amendments__seventeenth_amendment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(progressive_era_amendments__seventeenth_amendment, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(progressive_era_amendments__seventeenth_amendment, tangled_rope).
narrative_ontology:human_readable(progressive_era_amendments__seventeenth_amendment, "Seventeenth Amendment: Senate Elections from Legislatures to People").
narrative_ontology:topic_domain(progressive_era_amendments__seventeenth_amendment, "political/legal/constitutional").

domain_priors:requires_active_enforcement(progressive_era_amendments__seventeenth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(progressive_era_amendments__seventeenth_amendment, 'ff8eb120-9613-4e32-afed-bd0c0a335a6a').
narrative_ontology:cs_kernel_codification('ff8eb120-9613-4e32-afed-bd0c0a335a6a', formalized).
narrative_ontology:cs_authority_grounding('ff8eb120-9613-4e32-afed-bd0c0a335a6a', lineage).
narrative_ontology:cs_interpretation_layer_present('ff8eb120-9613-4e32-afed-bd0c0a335a6a').
narrative_ontology:cs_reading_relation('ff8eb120-9613-4e32-afed-bd0c0a335a6a', progressive_era_amendments__sixteenth_amendment, influences).
narrative_ontology:cs_reading_relation('ff8eb120-9613-4e32-afed-bd0c0a335a6a', progressive_era_amendments__eighteenth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('ff8eb120-9613-4e32-afed-bd0c0a335a6a', progressive_era_amendments__nineteenth_amendment, influences).
narrative_ontology:cs_axiom('ff8eb120-9613-4e32-afed-bd0c0a335a6a', foundational, legislature_mediated_senate_selection_is_corrupt).
narrative_ontology:cs_axiom_status(legislature_mediated_senate_selection_is_corrupt, holdable).
narrative_ontology:cs_axiom_grounding('ff8eb120-9613-4e32-afed-bd0c0a335a6a', legislature_mediated_senate_selection_is_corrupt, empirically_contingent).
narrative_ontology:cs_axiom('ff8eb120-9613-4e32-afed-bd0c0a335a6a', foundational, direct_election_eliminates_mediated_corruption).
narrative_ontology:cs_axiom_status(direct_election_eliminates_mediated_corruption, holdable).
narrative_ontology:cs_axiom_grounding('ff8eb120-9613-4e32-afed-bd0c0a335a6a', direct_election_eliminates_mediated_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('ff8eb120-9613-4e32-afed-bd0c0a335a6a', democratic_representation_via_popular_sovereignty).
narrative_ontology:cs_drift_state('ff8eb120-9613-4e32-afed-bd0c0a335a6a', contemporary_campaign_finance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff8eb120-9613-4e32-afed-bd0c0a335a6a', '').
narrative_ontology:cs_kernel_id(progressive_era_amendments__seventeenth_amendment, progressive_era_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(progressive_era_amendments__seventeenth_amendment, statewide_electorates).
narrative_ontology:constraint_beneficiary(progressive_era_amendments__seventeenth_amendment, progressive_reform_coalition).
narrative_ontology:constraint_victim(progressive_era_amendments__seventeenth_amendment, state_party_machines).
narrative_ontology:constraint_victim(progressive_era_amendments__seventeenth_amendment, legislative_federalism).
narrative_ontology:constraint_victim(progressive_era_amendments__seventeenth_amendment, incumbent_senators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISENFRANCHISED VOTER (SNARE) — Trapped. Before the Seventeenth Amendment, ordinary citizens had no voice in U.S. Senate selection; state legislatures controlled the seat entirely. The extraction is structural and absolute: citizens bear the costs of Senate policy (tariffs, labor regulation, land policy) while having zero input into who represents them at that level. No exit options exist within the original constraint — the voter cannot arbitrage, cannot exit, cannot organize effectively. The amendment eliminates this snare by transferring power to the electorate itself.
constraint_indexing:constraint_classification(progressive_era_amendments__seventeenth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PROGRESSIVE REFORM COALITION (TANGLED ROPE) — Organized agents (muckrakers, women's suffrage organizations, labor reformers, anti-corruption groups) see the legislature-mediated Senate selection as a mixed constraint: it coordinates state-federal balance (a genuine function) but does so through extractive practices (vote-buying, bribery, quid-pro-quo). The coalition is constrained by the political difficulty of amending the Constitution but benefits from the coordination function the Senate itself provides. The amendment represents active enforcement of a new reading: direct election restores the coordination function while eliminating the extraction mechanism.
constraint_indexing:constraint_classification(progressive_era_amendments__seventeenth_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE STATE PARTY MACHINE (SNARE) — Powerful but targeted by the amendment. The party machines of the Gilded Age extracted substantial control over Senate seats through legislative mediation: they could broker deals, demand kickbacks, and control nomination of senators without interference from the broader electorate. This constraint delivers maximum extraction to the machine while suppressing the electorate's alternatives. From the machine's perspective, the amendment is a catastrophic snare collapse — they had been the snare's primary beneficiary. High suppression of alternatives (they could enforce discipline on legislators), but mobile exit options for individual machines (some adapt to direct elections, some decline). The classification flips when we invert the directionality: the amendment is a snare *on the machines*, not on voters.
constraint_indexing:constraint_classification(progressive_era_amendments__seventeenth_amendment, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE FEDERAL CONSTITUTIONAL SYSTEM (ROPE) — The amendment preserves the genuine coordination function of federalism: states retain their identity as bounded political communities, but the mechanism for selecting their federal representatives shifts from legislative mediation to direct popular election. The amendment is experienced by the constitutional system as a coordination improvement — it maintains federalism while eliminating the intermediary rent-seeking. Extractiveness is low because the system itself is arbitraged by multiple agents with competing claims. No single actor can extract maximum value; the constraint distributes power back to the electorate.
constraint_indexing:constraint_classification(progressive_era_amendments__seventeenth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE STATE LEGISLATURE'S RESIDUAL AUTHORITY (PITON) — The amendment eliminates the legislature's active role in Senate selection, but legislatures retain formal authority over election mechanics, ballot access, voter qualifications, and redistricting. The theater ratio is high: the legislature performs its 'role' in running federal elections, but the functional power (who gets selected) has migrated to the electorate. The constraint is a piton — a vestigial institutional form maintained through procedural necessity (someone must administer elections) even though the primary function has atrophied. Theater_ratio reflects this degradation: administrative machinery without genuine decision-making power.
constraint_indexing:constraint_classification(progressive_era_amendments__seventeenth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / FEDERALISM VIEW (MOUNTAIN) — From a civilizational vantage, the Seventeenth Amendment does not eliminate federalism; it reorganizes it. The mountain here is the claim that federal systems *necessarily* mediate selection through some layer of governance — the amendment does not escape this necessity, it merely reassigns which layer (people via direct election vs. legislatures as institutional intermediaries). A hard-line federalism view treats this as an irreducible structural feature: states must exist as bounded units, and their representation must be mediated somehow. The amendment is reframing, not escaping. However, this perspective risks naturalizing what is actually a contested reading of federalism — the structural data reveals that the amendment's extraction collapse violates the mountain thresholds.
constraint_indexing:constraint_classification(progressive_era_amendments__seventeenth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(progressive_era_amendments__seventeenth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(progressive_era_amendments__seventeenth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(progressive_era_amendments__seventeenth_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(progressive_era_amendments__seventeenth_amendment, TR),
    TR >= 0.70.

:- end_tests(progressive_era_amendments__seventeenth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 at T=15, down from 0.68 at T=0): The constraint initially represents severe extraction by state machines over ordinary citizens — they controlled Senate seat allocation with citizens bearing the consequences (tariffs, labor policy, land disposition) while having zero input. The measurement trajectory shows the amendment's gradual effect: extractiveness drops as direct election consolidates and the legislative mediation mechanism loses power. The final value (0.38) reflects residual extraction mechanisms that direct election does not eliminate (campaign finance influence, voter information asymmetries, redistricting effects on state political structure). The tangled_rope classification captures that genuine coordination functions (federalism, state representation) coexist with ongoing extraction mechanisms. Suppression (0.62): The amendment significantly reduces suppression by eliminating the legislature's monopoly on Senate selection, but suppression does not drop to rope levels (≤0.35) because new suppression mechanisms emerge — voter manipulation via media, campaign finance barriers to challengers, gerrymandering of state legislative districts that determine voter eligibility. The measurement trajectory shows suppression declining from legislative control (0.75) to stabilized democratic suppression (0.62). Theater ratio (0.45): Relatively low. The direct election mechanism has high functional transparency — voters see the candidates, cast ballots, see results. The theater is primarily around campaign rhetoric (candidates claiming state representation while pursuing national agendas), not institutional ceremony. Lower theater distinguishes this from the piton perspective, which sees legislatures as increasingly ceremonial administrators of election mechanics.
 *
 * PERSPECTIVAL GAP:
 *   The measurement of extractiveness depends entirely on whether we measure the constraint as it existed pre-1913 (the state machine's extraction apparatus over citizens) or post-1913 (the residual extraction mechanisms within direct elections). The core perspectival gap: the party machines experienced the amendment as a catastrophic snare collapse — they lost the rent mechanism entirely. Citizens experienced it as a snare-to-rope transition — they gained agency and reduced extraction. The Progressive coalition experienced it as tangled_rope throughout — they saw both the coordination function of federalism AND the extraction mechanism of machine politics, and the amendment as restoring the former while eliminating the latter. The federalism perspective sees rope — coordination maintained, extraction eliminated. The legislature perspective sees piton — ceremonial authority without functional power. The analytical observer risks seeing mountain (federalism as immutable) when the structural data shows the opposite: the extractiveness drop is massive, proving the constraint is not natural law but contingent institutional practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality shifts radically through the amendment. Pre-1913, citizens are full targets (d ≈ 0.95) — they bear costs (Senate policy) while having zero exit options or voice. State machines are beneficiaries (d ≈ 0.05) — they extract rent from mediation and have arbitrage options (adapt their brokering tactics). Post-1913, directionality inverts partially: citizens become mobile rather than trapped (d drops), though not to the arbitrage level — they now have exit (stop supporting a senator via voting) but still face suppression (campaign finance barriers, information asymmetries). Machines become partially victimized (d rises) — they can no longer extract at the same rate, though some adapt to new electoral dynamics. The analytical frame derives d from structural position (exit capacity + beneficiary/victim status). The amendment reorganizes these structural positions, which is why the classification persists (tangled_rope through the transition) while directionality values reverse.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival clarity. The question is not 'is the Seventeenth Amendment a rope (coordination) or a snare (extraction)?' but 'from whose perspective are we measuring?' The amendment simultaneously CREATES a rope for citizens (democratizes representation, coordinates federalism) and DESTROYS a snare for citizens (eliminates legislature-mediated extraction). From the machines' perspective, it TRANSFORMS their snare (mediated control) into a constraint that snares them instead. The tangled_rope classification captures the truth: the amendment coordinates federalism while suppressing old extraction mechanisms, but it simultaneously creates new suppression mechanisms (voter manipulation, campaign finance barriers). The residual extractiveness (0.38) is not a failure of the amendment — it reflects that direct elections do not eliminate all extraction mechanisms, only those dependent on legislative mediation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vote_buying_mechanism_elimination,
    'Did the Seventeenth Amendment actually eliminate the structural mechanism for vote-buying and bribery, or merely relocate it to a larger electorate that is harder (but not impossible) to corrupt?',
    'Historical comparison of bribery prosecution rates pre- and post-1913; analysis of whether vote-buying costs per senator increased proportionally to electorate size; examination of documented corruption cases in the early direct-election era',
    'If eliminated: extractiveness drops below the tangled_rope floor (ε < 0.30), reclassifying to Rope. If merely relocated: extractiveness remains stable, but the suppression mechanism shifts from legislative control to voter persuasion/information control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vote_buying_mechanism_elimination, empirical, 'Whether the amendment eliminated vote-buying or merely displaced it to larger electorates').

omega_variable(
    federalism_reorganization_vs_collapse,
    'Does the Seventeenth Amendment genuinely preserve federalism (states as distinct political units with federal representation), or does it dissolve the federal principle by making Senate elections national popular contests detached from state institutional structures?',
    'Doctrinal analysis of post-1913 federalism jurisprudence; comparison of state-differentiated vs. national-homogenized Senate campaigns; examination of whether ''state interests'' remain coherent as a political category post-amendment',
    'If federalism preserved: the mountain perspective is false, and the tangled_rope classification is correct — coordination function intact, extraction eliminated. If federalism dissolved: the amendment represents a structural transformation rather than refinement, reclassifying to rope (pure coordination without residual federalism structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_reorganization_vs_collapse, conceptual, 'Whether the amendment preserves federalism or dissolves it').

omega_variable(
    progressive_coalition_agenda_capture,
    'Was the Seventeenth Amendment an authentic anti-corruption reform or a Progressive coalition strategy to weaken state-level party control and increase federal executive power?',
    'Archival analysis of Progressive writings pre-1913; cross-correlation between Seventeenth Amendment advocacy and other centralizing reforms (income tax, Commerce Clause expansion); examination of whether subsequent Senate behavior diverged from pre-1913 state-responsive patterns',
    'If authentic reform: extractiveness genuinely drops from the citizen''s perspective. If strategic weakening: the amendment trades one extraction mechanism (legislative mediation) for another (federal executive control), reclassifying from tangled_rope to snare-on-citizens-via-federation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(progressive_coalition_agenda_capture, empirical, 'Whether the amendment was anti-corruption reform or Progressive power consolidation').

omega_variable(
    kernel_reading_contest,
    'What is the structural relationship between the Seventeenth Amendment (direct Senate election) and the Sixteenth Amendment (income tax authorization)? Does one foreclose the other, or do they coexist as complementary Progressive reforms?',
    'Historical analysis of Progressive coalition writings; examination of whether fiscal federalism (national income tax) was logically dependent on senatorial democra­tization; review of whether any amendment advocate explicitly framed the pair as mutually necessary',
    'If coexist_with: both readings are live within Progressive constitutionalism. If influences: Seventeenth enables fiscal centralization by weakening state party machines'' capacity to resist federal taxation. If forecloses: the income tax reading is incompatible with the distributed federalism the Seventeenth was meant to preserve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship to Sixteenth Amendment within kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(progressive_era_amendments__seventeenth_amendment, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(seventeenth_extractiveness_preratification, progressive_era_amendments__seventeenth_amendment, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(seventeenth_extractiveness_early_direct, progressive_era_amendments__seventeenth_amendment, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(seventeenth_extractiveness_consolidated, progressive_era_amendments__seventeenth_amendment, base_extractiveness, 15, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seventeenth_suppression_legislative_control, progressive_era_amendments__seventeenth_amendment, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(seventeenth_suppression_transition, progressive_era_amendments__seventeenth_amendment, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(seventeenth_suppression_stabilized, progressive_era_amendments__seventeenth_amendment, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(progressive_era_amendments__seventeenth_amendment, resource_allocation).
narrative_ontology:affects_constraint(progressive_era_amendments__seventeenth_amendment, progressive_era_amendments__sixteenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__seventeenth_amendment, progressive_era_amendments__eighteenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__seventeenth_amendment, progressive_era_amendments__nineteenth_amendment).

% DUAL FORMULATION NOTE:
% The Seventeenth Amendment is one reading within the progressive_era_amendments kernel. It claims to eliminate extraction (purchased Senate seats) through institutional transparency (direct election). This reading coexists with the Sixteenth Amendment reading (which enables a new fiscal extraction apparatus) and influences the Nineteenth Amendment reading (women's suffrage becomes meaningful only when Senate elections are democratic). The constraint family is linked by their shared diagnosis of Gilded Age corruption and their claim that constitutional amendments can fix structural problems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(progressive_era_amendments__seventeenth_amendment, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
