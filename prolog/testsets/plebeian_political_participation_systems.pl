% ============================================================================
% CONSTRAINT STORY: plebeian_political_participation_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plebeian_political_participation_systems, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: plebeian_political_participation_systems
 *   human_readable: Plebeian Political Participation Systems
 *   domain: political_economy/democratic_institutions
 *
 * SUMMARY:
 *   Plebeian political participation systems create a structural tension
 *   between the democratic promise of popular sovereignty and the
 *   institutional reality of elite gatekeeping, information asymmetries, and
 *   strategic preference aggregation. Voters are encouraged to participate as
 *   an exercise of political voice, yet the mechanisms translating that voice
 *   into policy outputs are substantially controlled by institutional actors
 *   with independent incentives. This constraint exhibits all six DR types
 *   from different perspectives. The same structural phenomenon — the
 *   aggregation of millions of individual preferences into binding policy —
 *   appears as an immutable law of democratic aggregation (mountain), a
 *   genuine coordination mechanism enabling collective voice (rope), a mixed
 *   coordination-extraction hybrid with gatekeeping (tangled rope), a
 *   temporary institutional failure being solved by reform (scaffold), a
 *   degraded ritual persisting through inertia (piton), or pure extraction
 *   with minimal voice fidelity (snare), depending on the observer's
 *   structural position. The constraint's theater_ratio (0.68) reflects that
 *   contemporary electoral participation involves substantial performative
 *   elements: voting day rituals, civic mythology, candidate theater debates,
 *   and symbolic empowerment narratives that obscure the reality of
 *   agenda-setting power concentrated outside formal participation channels.
 *
 * KEY AGENTS:
 *   - Non-Elite Voters: Primary victim (powerless/trapped) — subject to participation obligations with minimal effective influence; bear costs of gatekeeping and suppression
 *   - Political Voice Equity: Primary victim (powerless/trapped) — abstract collective good; extractive participation systems degrade democratic legitimacy and representation quality
 *   - Party Apparatus: Primary beneficiary (institutional/arbitrage) — captures electoral value, donor access, legislative power; can arbitrage between constituencies and shift positions
 *   - Electoral Gatekeepers: Primary beneficiary (institutional/arbitrage) — control ballot access, voter registration, information flow, agenda-setting; extract rents from participation monopoly
 *   - Organized Community Groups: Secondary actor (moderate/constrained) — benefit from collective participation power but subject to extraction via co-optation and institutional incorporation
 *   - Democratic Reform Coalition: Organized agents (organized/mobile) — civil society, election monitoring, voter mobilization networks building alternative participation pathways with generational sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional gatekeeping as inherent limit of democratic aggregation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plebeian_political_participation_systems, 0.58).
domain_priors:suppression_score(plebeian_political_participation_systems, 0.65).
domain_priors:theater_ratio(plebeian_political_participation_systems, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plebeian_political_participation_systems, extractiveness, 0.58).
narrative_ontology:constraint_metric(plebeian_political_participation_systems, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(plebeian_political_participation_systems, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plebeian_political_participation_systems, tangled_rope).
narrative_ontology:human_readable(plebeian_political_participation_systems, "Plebeian Political Participation Systems").
narrative_ontology:topic_domain(plebeian_political_participation_systems, "political_economy/democratic_institutions").

domain_priors:requires_active_enforcement(plebeian_political_participation_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plebeian_political_participation_systems, electoral_gatekeepers).
narrative_ontology:constraint_beneficiary(plebeian_political_participation_systems, party_apparatus).
narrative_ontology:constraint_beneficiary(plebeian_political_participation_systems, institutional_politicians).
narrative_ontology:constraint_victim(plebeian_political_participation_systems, non_elite_voters).
narrative_ontology:constraint_victim(plebeian_political_participation_systems, political_voice_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED VOTER (SNARE) — Participates in electoral system with minimal effective influence over outcomes. Structural barriers include ballot access requirements, voter registration systems, polling place accessibility, gerrymandering, and information asymmetries. No viable exit: political voice is essential to citizenship. Maximum experienced extraction — participation becomes an obligation without meaningful agency.
constraint_indexing:constraint_classification(plebeian_political_participation_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED COMMUNITY GROUP (TANGLED ROPE) — Genuine coordination benefit: collective participation builds political power, enables advocacy campaigns, develops civic capacity. But also subject to extraction: must navigate gatekeeping mechanisms, absorb organizational costs, face co-optation pressure and incorporation into party structures that dilute autonomous voice. Constrained exit — withdrawing from participation system costs political influence, but participation incurs coordination costs and agency loss.
constraint_indexing:constraint_classification(plebeian_political_participation_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POLITICAL PARTY APPARATUS (ROPE) — Experiences the participation constraint as coordination mechanism: aggregating plebeian votes into electoral blocs solves collective action problem for the party. Can arbitrage between constituencies, shift positions, and rotate power. Net beneficiary — extraction flows toward the party, which captures electoral value, donor access, and legislative power.
constraint_indexing:constraint_classification(plebeian_political_participation_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM COALITION (SCAFFOLD) — Sees participation system as temporary problem with sunset: expanded suffrage, campaign finance reform, redistricting commissions, participatory budgeting, and digital participation tools are building alternative pathways that bypass traditional gatekeeper extraction. Organized agents (civil society, election monitoring, voter mobilization networks) perceive this as a coordination failure being solved by institutional redesign. Sunset clause is real: as voting-age population expands and inclusion norms mature, the exclusionary barrier mechanisms lose legitimacy and enforcement capacity.
constraint_indexing:constraint_classification(plebeian_political_participation_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ELECTORAL RITUAL SYSTEM (PITON) — The machinery of voting — polling places, ballot design, procedural voting requirements, counting and certification — persists through institutional inertia. Much of the contemporary electoral process is performative: voting day rituals, civic mythology around 'the power of your vote,' candidate debates with predetermined formats. Actual agenda-setting often occurs outside formal electoral participation (lobbying, media gatekeeping, donor influence). Theater ratio high because the ritual persists despite reduced functional verification of voter preferences.
constraint_indexing:constraint_classification(plebeian_political_participation_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some collective action problem in political coordination is inherent: any system aggregating diverse preferences faces Arrow's impossibility result, strategic voting, and preference revelation problems. Plebeian participation systems always involve some extraction of voice into institutional forms. This perspective sees the bottleneck as a structural limit of democratic aggregation itself. However, the base properties contradict this — the constraint is contingent institutional arrangement, not natural law. The mountain classification is a false summit that naturalizes what is actually a distributional choice.
constraint_indexing:constraint_classification(plebeian_political_participation_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plebeian_political_participation_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(plebeian_political_participation_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plebeian_political_participation_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(plebeian_political_participation_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(plebeian_political_participation_systems, TR),
    TR >= 0.70.

:- end_tests(plebeian_political_participation_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The gatekeeper apparatus captures substantial value — electoral power, media attention, donor access, agenda control — during the participation cycle. But the extraction is not maximal because genuine coordination also occurs: electoral competition does constrain some elite behavior, voters do punish incumbent failures at margins, and alternative participation channels (organizing, protest, media, direct democracy) partially bypass formal gatekeeping. The value reflects both the real benefits of participation (voice amplification through collective aggregation) and the real costs (gatekeeping extraction, voice filtering). Suppression (0.65): Moderate-high. Significant barriers include voter registration requirements and delays, polling place accessibility gaps, ballot design complexity, gerrymandering, information asymmetries, and media gatekeeping. However, suppression is not total — millions do participate effectively, and suppression mechanisms are increasingly visible and contestable. Theater ratio (0.68): Moderately high. Electoral participation involves substantial performative elements: voting day rituals, media spectacle around candidates, predetermined debate formats, victory narratives. Actual policy agenda-setting often occurs in less participatory forums (lobbying, board rooms, regulatory capture). The theater has increased over the interval (0.42 to 0.68) as electoral competition has intensified while agenda-setting power has concentrated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival diversity. The non-elite voter sees pure extraction (Snare) — participation obligations with minimal voice fidelity. The community organization sees mixed coordination-extraction (Tangled Rope) — collective participation amplifies power but gatekeeper incorporation reduces autonomy. The party apparatus sees coordination (Rope) — voters solve their collective action problem by channeling voice through the party. The reform coalition sees a solvable institutional problem (Scaffold) — expanding suffrage, decentralizing agenda-setting, and building participatory budgeting create alternative pathways. The electoral ritual system sees its own degradation (Piton) — voting day theater persists despite agenda-setting occurring elsewhere. The civilizational observer risks seeing immutable democratic limits (Mountain) — Arrow's impossibility and collective action problems are inherent. This last perspective is a false summit: the structural barriers are institutional, not mathematical.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the gatekeeping apparatus. Voters with no exit options (trapped) face maximum extraction — the gatekeepers control when, where, and how voting occurs, and voters bear the participation costs. Party gatekeepers with arbitrage options (institutional/arbitrage) experience extraction flowing toward them — they aggregate votes into power and can shift positions. Organized community groups with some mobility (moderate/constrained) face mixed directionality — they benefit from participation coordination but lose autonomy through institutional incorporation. The reform coalition with mobile options sees this as a solvable institutional problem, not extractive fate. The beneficiary classification differentiates institutional gatekeepers (arbitrage capacity) from powerful but non-gatekeeping actors.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: This constraint demonstrates mandatrophy by showing that the six types are legitimate readings of the same participation system from different structural positions. The constraint does not resolve to a single 'true' type but rather shows how the same formal participation institution generates different extractiveness experiences depending on the actor's power level and exit options. The mountain perspective naturalizes what is actually a distributional choice (gatekeeping design). The rope perspective sees only the coordination benefit while ignoring extraction. The snare perspective sees only extraction while ignoring genuine voice amplification. The tangled rope perspective is most accurate from the analytical observer's position — plebeian participation systems genuinely coordinate collective voice while simultaneously extracting via gatekeeping. The scaffold and piton perspectives are real structural features of contemporary systems (reform movements and electoral theater). The mandatrophy is resolved by recognizing that the indexical tuple fully determines which type is experientially accurate from that position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_mechanism_taxonomy,
    'Is suppression of plebeian participation primarily via structural barriers (registration, access, ballot design) or via elite preference aggregation (media, donor influence, agenda-setting)?',
    'Comparative analysis of participation rates after removing each barrier class; measurement of agenda difference between elite and mass preferences pre/post participation channels',
    'If structural: focused reform on access removes extraction (Rope emerges from more perspectives). If aggregation-based: structural reform fails to transfer voice unless preference aggregation mechanisms change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_mechanism_taxonomy, empirical, 'Taxonomy of gatekeeping mechanisms in plebeian participation').

omega_variable(
    voice_transfer_fidelity,
    'Does plebeian participation in electoral systems actually transmit voter preferences to policy outputs, or does institutional filtering decouple voting from outcomes?',
    'Policy congruence analysis: correlation between voter preferences (pre-election polls, referenda, issue surveys) and actual policy decisions by elected representatives; tracking of campaign promises vs implementation',
    'High fidelity (ρ > 0.7): participation is genuine coordination, classification shifts toward Rope from more perspectives. Low fidelity (ρ < 0.3): participation is performative extraction, Snare classification spreads.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voice_transfer_fidelity, empirical, 'Fidelity of voice transfer from participation to policy').

omega_variable(
    comparative_system_extractiveness,
    'Is high extractiveness in this participation system attributable to universal structural properties of democratic aggregation or to contingent institutional design choices?',
    'Cross-national comparison of extractiveness measures between systems with different institutional designs (proportional vs first-past-the-post, centralized vs local, party-gatekept vs direct-democracy hybrid)',
    'If universal: mountain classification has merit, extractiveness is inherent. If contingent: extractiveness correlates with specific institutional choices, reform is feasible and constraint is Tangled Rope or Scaffold rather than Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_system_extractiveness, empirical, 'Whether extractiveness is universal or contingent on institutional design').

omega_variable(
    exit_option_availability,
    'What constitutes genuine exit from the political participation constraint for powerless agents? Is exit possible (mobile/arbitrage) or are barriers truly insurmountable (trapped)?',
    'Measurement of political exit costs: relocation burden, citizenship loss, alternative political systems availability, cost of permanent disengagement (taxation without representation impact)',
    'High-cost exit (>50% of lifetime earnings): trapped classification justified, Snare perspective accurate. Low-cost exit: mobile classification applies, higher d value, lower experienced chi, classification shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Availability of genuine exit options from political participation systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plebeian_political_participation_systems, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pleb_part_tr_t0, plebeian_political_participation_systems, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pleb_part_tr_t20, plebeian_political_participation_systems, theater_ratio, 20, 0.55).
narrative_ontology:measurement(pleb_part_tr_t40, plebeian_political_participation_systems, theater_ratio, 40, 0.68).
narrative_ontology:measurement(pleb_part_tr_t10, plebeian_political_participation_systems, theater_ratio, 10, 0.49).

% Extraction over time
narrative_ontology:measurement(pleb_part_be_t0, plebeian_political_participation_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pleb_part_be_t20, plebeian_political_participation_systems, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(pleb_part_be_t40, plebeian_political_participation_systems, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(pleb_part_be_t10, plebeian_political_participation_systems, base_extractiveness, 10, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plebeian_political_participation_systems, identity_coordination).
narrative_ontology:affects_constraint(plebeian_political_participation_systems, voter_preference_aggregation).
narrative_ontology:affects_constraint(plebeian_political_participation_systems, democratic_representation_fidelity).
narrative_ontology:affects_constraint(plebeian_political_participation_systems, electoral_gatekeeping_power).

% DUAL FORMULATION NOTE:
% Plebeian political participation systems decompose into multiple structurally distinct constraints: formal voting mechanisms (ε≈0.55, Tangled Rope), information aggregation via media (ε≈0.62, Snare), agenda-setting concentration (ε≈0.71, Snare), and voice-policy translation fidelity (ε≈0.58, Tangled Rope). This story models the integrated system; specific institutional components should be analyzed as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plebeian_political_participation_systems, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
