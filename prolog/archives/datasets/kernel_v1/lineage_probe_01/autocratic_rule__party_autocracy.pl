% ============================================================================
% CONSTRAINT STORY: autocratic_rule__party_autocracy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autocratic_rule__party_autocracy, []).

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
 *   constraint_id: autocratic_rule__party_autocracy
 *   human_readable: Party Autocracy: Single-Party Monopoly on Political Organization
 *   domain: political/comparative_governance
 *
 * SUMMARY:
 *   Party autocracy — the single-party monopoly on organized politics — is
 *   one reading of how autocratic rule stabilizes itself. This reading claims
 *   that autocracy's distinctive organized form is a disciplined party
 *   apparatus that manages succession, legitimacy, and careers inside the
 *   system while suppressing all external competition. The party monopoly
 *   solves the succession problem (no throne wars between rival factions) and
 *   provides ideological legitimacy (the party claims to represent the
 *   people/nation/historical progress). But this coordination function
 *   coexists with severe extraction: the nomenklatura as a class benefits
 *   from monopoly power, while non-members are locked out of political
 *   influence and mid-rank members face forced conformity and extraction. The
 *   single party is neither pure coordination (Rope) nor pure extraction
 *   (Snare) — it is Tangled Rope: the party genuinely coordinates succession
 *   and elite stability while simultaneously extracting compliance and
 *   suppressing alternatives. The constraint's extractiveness has increased
 *   over the interval (0.35 → 0.58) as the apparatus has consolidated and
 *   suppression has hardened, suggesting that over time the coordination
 *   function becomes ornamental and extraction becomes dominant. The theater
 *   ratio (rising from 0.55 to 0.65) reflects that party rituals —
 *   congresses, plenums, ideological certifications — have become
 *   increasingly performative: they ratify decisions made in inner circles
 *   rather than distributing power. This reading coexists with sibling
 *   readings (hereditary monarchy, military junta, personalist dictatorship)
 *   that each claim different organizing principles for autocratic rule.
 *
 * KEY AGENTS:
 *   - Nomenklatura Elite: Primary beneficiary (institutional/arbitrage) — benefits from party monopoly guarantee of power, succession stability, controlled access to state resources
 *   - Non-Party Citizens: Primary victim (powerless/trapped) — locked out of political influence and career advancement in state institutions; exit options constrained by dependence on state employment
 *   - Mid-Rank Party Members: Secondary victim (moderate/constrained) — benefit from party career pathway but face extraction through loyalty demands, purge risk, ideological conformity requirements
 *   - Inner-Party Losers: Secondary victim (moderate/trapped) — party members who fall out of favor face demotion, purge, or exile with no exit pathway outside the apparatus
 *   - Organized Opposition: Tertiary victim (organized/constrained) — outside the monopoly, suppressed from organizing, but also benefits from clear definition of antagonist (the party defines opposition through negation)
 *   - Party Apparatus Itself: Institutional structure (institutional/arbitrage) — the formal rituals and mechanisms that perform legitimacy while real power concentrates in inner circles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autocratic_rule__party_autocracy, 0.58).
domain_priors:suppression_score(autocratic_rule__party_autocracy, 0.78).
domain_priors:theater_ratio(autocratic_rule__party_autocracy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autocratic_rule__party_autocracy, extractiveness, 0.58).
narrative_ontology:constraint_metric(autocratic_rule__party_autocracy, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(autocratic_rule__party_autocracy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autocratic_rule__party_autocracy, tangled_rope).
narrative_ontology:human_readable(autocratic_rule__party_autocracy, "Party Autocracy: Single-Party Monopoly on Political Organization").
narrative_ontology:topic_domain(autocratic_rule__party_autocracy, "political/comparative_governance").

domain_priors:requires_active_enforcement(autocratic_rule__party_autocracy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(autocratic_rule__party_autocracy, '7a886b73-b17c-4985-912b-79ebb212a75c').
narrative_ontology:cs_kernel_codification('7a886b73-b17c-4985-912b-79ebb212a75c', distributed).
narrative_ontology:cs_authority_grounding('7a886b73-b17c-4985-912b-79ebb212a75c', extraction).
narrative_ontology:cs_interpretation_layer_present('7a886b73-b17c-4985-912b-79ebb212a75c').
narrative_ontology:cs_reading_relation('7a886b73-b17c-4985-912b-79ebb212a75c', autocratic_rule__hereditary_monarchy, coexists_with).
narrative_ontology:cs_reading_relation('7a886b73-b17c-4985-912b-79ebb212a75c', autocratic_rule__military_junta, coexists_with).
narrative_ontology:cs_reading_relation('7a886b73-b17c-4985-912b-79ebb212a75c', autocratic_rule__personalist_dictatorship, influences).
narrative_ontology:cs_axiom('7a886b73-b17c-4985-912b-79ebb212a75c', foundational, succession_via_party_apparatus).
narrative_ontology:cs_axiom_status(succession_via_party_apparatus, holdable).
narrative_ontology:cs_axiom_grounding('7a886b73-b17c-4985-912b-79ebb212a75c', succession_via_party_apparatus, conventional).
narrative_ontology:cs_axiom('7a886b73-b17c-4985-912b-79ebb212a75c', foundational, party_monopoly_coordination_function).
narrative_ontology:cs_axiom_status(party_monopoly_coordination_function, holdable).
narrative_ontology:cs_axiom_grounding('7a886b73-b17c-4985-912b-79ebb212a75c', party_monopoly_coordination_function, instrumental).
narrative_ontology:cs_reference_frame('7a886b73-b17c-4985-912b-79ebb212a75c', institutionalized_party_rule).
narrative_ontology:cs_drift_state('7a886b73-b17c-4985-912b-79ebb212a75c', contemporary_erosion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7a886b73-b17c-4985-912b-79ebb212a75c', '').
narrative_ontology:cs_kernel_id(autocratic_rule__party_autocracy, autocratic_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autocratic_rule__party_autocracy, nomenklatura_class).
narrative_ontology:constraint_beneficiary(autocratic_rule__party_autocracy, party_leadership).
narrative_ontology:constraint_victim(autocratic_rule__party_autocracy, non_party_members).
narrative_ontology:constraint_victim(autocratic_rule__party_autocracy, inner_party_losers).
narrative_ontology:constraint_victim(autocratic_rule__party_autocracy, competing_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-PARTY CITIZEN (SNARE) — Citizens outside the party apparatus have no structural path to political influence or career advancement in state institutions. Exit from the political system entirely is constrained by geography and economic dependence on state employment. The party monopoly extracts compliance and loyalty while offering zero influence. Maximum suppression, maximum extraction.
constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-RANK PARTY MEMBER (TANGLED ROPE) — Mid-rank cadres benefit from party membership (career security, access to state resources, professional community) but face extraction through mandatory party discipline, ideological conformity requirements, and risk of purge or demotion if they fall out of favor with superiors. Exit from the party costs career advancement and social position. Mixed coordination (party provides career pathway) and extraction (party demands loyalty and conformity).
constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NOMENKLATURA ELITE (ROPE) — Senior party members and nomenklatura experience the single-party system as a coordination mechanism that delivers succession stability, ideological legitimacy, and career predictability. They benefit from the monopoly (guaranteed power, no external competition, controlled circulation through ranks). The party apparatus solves their coordination problem: how to manage succession and prevent throne wars. For this group, the system functions as pure coordination.
constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPPOSITION MOVEMENT (TANGLED ROPE) — Organized opposition groups outside the party face suppression (bans on organizing, surveillance, arrest) but also benefit from the single-party system's structural clarity — the party defines itself through opposition to them, which can paradoxically sustain opposition identity and mobilization. Opposition also derives delegitimacy from the party's monopoly claim: if the party claims to represent all political will, opposition must prove it doesn't. The constraint is mixed: severe suppression of organizing capacity, but also provides opposition with a clear antagonist and legitimacy challenge.
constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DIASPORA ELITE (TANGLED ROPE) — Diaspora members with mobile exit options (international professional mobility, external funding, global media platforms) can coordinate opposition to the party autocracy while enjoying safety of exit. They benefit from some party system functions (clear definition of the regime they oppose, predictable organizational structure of their antagonist) but experience extraction through suppression of family members, asset seizure, exile pressure. Mixed experience: significant agency through exit mobility, but ongoing extraction through hostage effects.
constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: RITUALS OF PARTY CONGRESSES (PITON) — The formal institutions of the party (congresses, plenums, ideological certifications) are substantially theatrical: they ratify decisions made in private, perform consensus, and exist primarily to legitimize elite decisions rather than to distribute power. The party's formal apparatus persists through institutional inertia long after real power has consolidated in a smaller core. Theater ratio is high — the rituals still function as performances of democratic procedure within the party elite, but their actual decision-making role has atrophied.
constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a purely comparative-historical stance, some degree of party monopoly appears as an immutable feature of autocratic rule across centuries and cultures: every successful autocracy develops an organizing apparatus to manage succession and prevent internal fragmentation. The single-party form appears as a natural law of political organization under authoritarian constraint. This perspective risks naturalizing what is structurally a contingent institutional arrangement maintained through suppression and extracted loyalty.
constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autocratic_rule__party_autocracy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autocratic_rule__party_autocracy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(autocratic_rule__party_autocracy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(autocratic_rule__party_autocracy, TR),
    TR >= 0.70.

:- end_tests(autocratic_rule__party_autocracy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The party autocracy extracts from non-members (complete political exclusion), from mid-rank members (forced loyalty and conformity), and from society (monopoly suppression). But the extraction is not maximal (as it would be in a pure Snare like personalist dictatorship) because the party provides genuine coordination services: it manages succession without throne wars, provides career pathways for ambitious members, and distributes power through nomenklatura rotation rather than concentrating it in one person's hands. The rising extractiveness over the interval (0.35 → 0.58) reflects that as the apparatus consolidates, extraction mechanisms harden and coordination benefits narrow to smaller elite circles. Suppression (0.78): High. Legal bans on outside organizing, surveillance of non-party groups, arrest of opposition, controls on speech and association. The suppression is structural (law-based) not merely coercive (violence alone). Theater ratio (0.65): Moderate-high. Party congresses, plenums, ideological meetings, and certification rituals perform legitimacy (consensus, representation, participation) while actual power is concentrated in steering committees and informal networks. The theater has increased over the interval as elite consolidation makes the formal apparatus more ornamental. Claimed type (Tangled Rope): The constraint has genuine coordination function (succession, elite stability, career pathways) AND asymmetric extraction (non-members excluded, members suppressed). Requires active enforcement (suppression apparatus). Beneficiaries (nomenklatura class, party leadership) and victims (non-members, inner-party losers, competing organizations) are clearly distinguished.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects the split between those inside and outside the party monopoly, and between different ranks within it. The non-party citizen sees a snare (complete political exclusion, trapped). The mid-rank member sees tangled rope (career benefits mixed with extraction). The nomenklatura elite sees rope (pure coordination and guaranteed power). The opposition movement sees tangled rope (suppression mixed with clarity of antagonist). The diaspora elite sees tangled rope (mobile exit mixed with hostage effects on family). The party rituals appear as piton (performative apparatus). The civilizational observer risks seeing mountain (naturalized as immutable law of autocratic organization). The engine will likely flag the mountain perspective as false summit — the constraint is contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's power level, exit options, and relationship to the party monopoly. Non-party citizens: powerless/trapped → high d → high extraction experienced. Mid-rank members: moderate/constrained → moderate d → mixed experience. Nomenklatura: institutional/arbitrage → low d → low extraction (net beneficiary). Opposition: organized/constrained → moderate d → mixed (suppression + clarity). Diaspora: powerful/mobile → low-to-moderate d → can exit but faces extraction through family hostages. The party apparatus itself: institutional/arbitrage → low d (the apparatus is the beneficiary). The civilization observer: analytical/analytical → moderate d → at-risk position that naturalizes the constraint. The beneficiary/victim declarations feed the directionality chain: nomenklatura are declared beneficiaries, driving their d downward; non-members and inner-party losers are declared victims, driving their d upward. The resulting perspectival gap is the signature of Tangled Rope at analytical level.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the party apparatus is genuinely a Tangled Rope (not pure coordination or pure extraction) because it provides real succession coordination while simultaneously extracting loyalty and suppressing alternatives. The mandatrophy would arise if we tried to call it pure Rope (the beneficiary perspective) or pure Snare (the powerless perspective). The analytical resolution is that it is legitimately Tangled Rope: the same apparatus that solves elite succession simultaneously solves non-elite exclusion. The rising extractiveness over the interval (0.35 → 0.58) suggests that over historical time, the coordination function may degrade and extraction may become dominant — a drift toward Snare. This would be captured by future measurement intervals and could trigger reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    party_apparatus_exit_barrier_structural_vs_internalized,
    'Is suppression of non-party organizing primarily structural (legal bans, violence, surveillance) or internalized (citizens believe party monopoly is legitimate or inevitable)?',
    'Comparative analysis across party autocracies: countries with relaxed suppression (still legal monopoly but reduced enforcement) show whether non-party organizing spontaneously emerges. Exit surveys of defectors reveal whether they cite external barriers or internalized legitimacy.',
    'If primarily structural: suppression floor is accurate and constraint can be reduced by enforcement withdrawal. If primarily internalized: suppression persists after structural removal, and the constraint''s true mechanism is deeper (hegemonic ideology, not force alone).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_apparatus_exit_barrier_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    nomenklatura_extraction_mechanism,
    'Does the cadre system actually extract value from mid-rank party members (through loyalty demands, unpaid labor, suppression of exit), or does it genuinely distribute career security and state resource access?',
    'Comparative income/wealth analysis of party members vs non-members; career interruption rates from purges; measurements of actual influence mid-rank members exercise over policy.',
    'If extraction exceeds coordination benefit: constraint is Snare from mid-rank perspective. If coordination benefit exceeds extraction: constraint is Rope from mid-rank perspective. Mixed outcome confirms Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nomenklatura_extraction_mechanism, empirical, 'Relative extraction vs coordination benefit for mid-rank cadres').

omega_variable(
    succession_stability_counterfactual,
    'Does the single-party apparatus actually reduce succession crises, or does it create predictable cycles of factional struggle between competing party elite factions?',
    'Historical comparison of succession transitions in party autocracies vs monarchies vs personalist dictatorships; measurement of power concentration shifts during transitions; elite purge rates post-succession.',
    'If genuinely stabilizing: party apparatus is solving a real coordination problem and Rope classification is justified. If merely redistributing struggle to intra-party domain: the party is Snare that concentrates extraction within elite while appearing to stabilize succession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_counterfactual, empirical, 'Whether single-party apparatus reduces succession crises or concentrates them intra-party').

omega_variable(
    reading_boundary_party_vs_personalist,
    'Is this reading (party autocracy) distinct from the personalist dictatorship reading, or are ''party'' monopolies in practice ruled by a single person above the party machinery?',
    'Structural analysis of leadership succession mechanisms: does a new leader inherit the party machinery and must work through it, or does the person simply take control and party apparatus becomes ornamental?',
    'If party genuinely constrains the leader: this is a distinct reading with genuine Rope/Tangled Rope dynamics. If the leader rules above/through/against the party: the reading collapses into personalist dictatorship and extractiveness rises significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_party_vs_personalist, conceptual, 'Whether party autocracy is structurally distinct from personalist dictatorship').

omega_variable(
    alternative_autocratic_forms_reading_stability,
    'Are the sibling readings (monarchy, junta, personalism) genuinely distinct equilibria that a state can transition between, or are they different analytical frames on the same underlying monopoly?',
    'Historical case analysis of regimes that shifted between forms (e.g., post-party states that revert to military or personalist rule, monarchies that institutionalize party mechanisms). Do the transitions represent genuine structural changes or reframing of the same extraction apparatus?',
    'If genuinely distinct equilibria: this reading''s boundary is real and the kernel contest is meaningful. If reframing: the readings may collapse under alternative analytic framing and the ε-invariance principle requires decomposition into sub-constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_autocratic_forms_reading_stability, conceptual, 'Whether sibling readings are distinct equilibria or analytic frames').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autocratic_rule__party_autocracy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(party_auto_tr_t0, autocratic_rule__party_autocracy, theater_ratio, 0, 0.55).
narrative_ontology:measurement(party_auto_tr_t10, autocratic_rule__party_autocracy, theater_ratio, 10, 0.6).
narrative_ontology:measurement(party_auto_tr_t20, autocratic_rule__party_autocracy, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(party_auto_be_t0, autocratic_rule__party_autocracy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(party_auto_be_t10, autocratic_rule__party_autocracy, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(party_auto_be_t20, autocratic_rule__party_autocracy, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(party_auto_su_t0, autocratic_rule__party_autocracy, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(party_auto_su_t10, autocratic_rule__party_autocracy, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(party_auto_su_t20, autocratic_rule__party_autocracy, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autocratic_rule__party_autocracy, enforcement_mechanism).
narrative_ontology:affects_constraint(autocratic_rule__party_autocracy, autocratic_rule__hereditary_monarchy).
narrative_ontology:affects_constraint(autocratic_rule__party_autocracy, autocratic_rule__military_junta).
narrative_ontology:affects_constraint(autocratic_rule__party_autocracy, autocratic_rule__personalist_dictatorship).
narrative_ontology:affects_constraint(autocratic_rule__party_autocracy, nomenklatura_circulation_extraction).
narrative_ontology:affects_constraint(autocratic_rule__party_autocracy, party_ideology_legitimacy_claim).

% DUAL FORMULATION NOTE:
% This constraint is linked to sibling readings of the same kernel (autocratic_rule). The three sibling stories (monarchy, junta, personalism) represent competing claims about autocratic organization. Party_autocracy is one claim; sibling readings are others. The network affects_constraints lists both the sibling readings (required for kernel family coherence) and downstream constraints that depend on this reading (nomenklatura circulation, ideology legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
