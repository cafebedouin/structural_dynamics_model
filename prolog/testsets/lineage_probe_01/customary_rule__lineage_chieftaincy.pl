% ============================================================================
% CONSTRAINT STORY: customary_rule__lineage_chieftaincy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_customary_rule__lineage_chieftaincy, []).

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
 *   constraint_id: customary_rule__lineage_chieftaincy
 *   human_readable: Customary Rule: Lineage Chieftaincy Authority Structure
 *   domain: political/comparative/customary_institutions
 *
 * SUMMARY:
 *   Customary rule organized through chieftaincy represents a specific
 *   institutional reading of descent-based political authority. In this
 *   reading, the constraint is the political form itself: authority held by
 *   recognized lineages, exercised through council and consensus, with the
 *   office (the chiefdom) persisting independent of any individual holder.
 *   The constraint structures who can claim legitimate authority, what
 *   obligations and extraction they can impose, and who bears the costs. This
 *   is one of three structurally distinct readings of 'customary rule': the
 *   lineage chieftaincy reading (political form), the customary land tenure
 *   reading (economic form grounded in community membership), and the elder
 *   adjudication reading (judicial form oriented toward reconciliation).
 *   These readings coexist but are not identical — a society can have
 *   chieftaincy authority without customary land tenure, or elder
 *   adjudication without chiefly councils. This story models ONLY the lineage
 *   chieftaincy reading, treating it as a distinct constraint with its own
 *   extractiveness, beneficiary/victim structure, and perspectives.
 *
 * KEY AGENTS:
 *   - Recognized Lineages: Primary beneficiary (institutional/arbitrage) — hold exclusive claim to chiefly office; control tribute collection and redistribution; benefit from institutional legitimacy and authority veto over strangers and junior lines
 *   - Junior Lineage Segments: Secondary victim (moderate/constrained) — have lineage standing but subordinate authority; pay tribute; excluded from senior council roles; constrained by seniority rules but not trapped (succession can elevate)
 *   - Strangers and Non-Lineage Members: Primary victim (powerless/trapped) — no legitimate authority claim regardless of competence; excluded by descent rule; trapped by lack of kinship standing and no exit mechanism short of leaving the community
 *   - State Authority (Colonial or Post-Colonial): Secondary institutional actor (institutional/constrained) — benefits from chieftaincy as administrative tier but constrained by need to maintain institutional legitimacy; may see chieftaincy as useful or as obstacle depending on state capacity
 *   - Elder Moot / Judicial Parallel Authority: Institutional actor with uncertain relationship to chieftaincy — may be subordinate to chief, parallel with chief, or competing with chief depending on ethnographic context
 *   - Anthropological Observer: Analytical position (analytical/analytical) — risks naturalizing chieftaincy as immutable kinship logic or risks dismissing it as colonial invention without examining actual institutional capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(customary_rule__lineage_chieftaincy, 0.38).
domain_priors:suppression_score(customary_rule__lineage_chieftaincy, 0.52).
domain_priors:theater_ratio(customary_rule__lineage_chieftaincy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(customary_rule__lineage_chieftaincy, extractiveness, 0.38).
narrative_ontology:constraint_metric(customary_rule__lineage_chieftaincy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(customary_rule__lineage_chieftaincy, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(customary_rule__lineage_chieftaincy, tangled_rope).
narrative_ontology:human_readable(customary_rule__lineage_chieftaincy, "Customary Rule: Lineage Chieftaincy Authority Structure").
narrative_ontology:topic_domain(customary_rule__lineage_chieftaincy, "political/comparative/customary_institutions").

domain_priors:requires_active_enforcement(customary_rule__lineage_chieftaincy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(customary_rule__lineage_chieftaincy, 'a477be6a-26aa-44cf-a2a7-71ee78236ff5').
narrative_ontology:cs_kernel_codification('a477be6a-26aa-44cf-a2a7-71ee78236ff5', formalized).
narrative_ontology:cs_authority_grounding('a477be6a-26aa-44cf-a2a7-71ee78236ff5', lineage).
narrative_ontology:cs_interpretation_layer_present('a477be6a-26aa-44cf-a2a7-71ee78236ff5').
narrative_ontology:cs_reading_relation('a477be6a-26aa-44cf-a2a7-71ee78236ff5', customary_rule__land_tenure, coexists_with).
narrative_ontology:cs_reading_relation('a477be6a-26aa-44cf-a2a7-71ee78236ff5', customary_rule__elder_adjudication, coexists_with).
narrative_ontology:cs_axiom('a477be6a-26aa-44cf-a2a7-71ee78236ff5', foundational, descent_group_monopoly_authority).
narrative_ontology:cs_axiom_status(descent_group_monopoly_authority, holdable).
narrative_ontology:cs_axiom_grounding('a477be6a-26aa-44cf-a2a7-71ee78236ff5', descent_group_monopoly_authority, conventional).
narrative_ontology:cs_axiom('a477be6a-26aa-44cf-a2a7-71ee78236ff5', foundational, office_persistence_across_holders).
narrative_ontology:cs_axiom_status(office_persistence_across_holders, holdable).
narrative_ontology:cs_axiom_grounding('a477be6a-26aa-44cf-a2a7-71ee78236ff5', office_persistence_across_holders, conventional).
narrative_ontology:cs_axiom('a477be6a-26aa-44cf-a2a7-71ee78236ff5', secondary, consensus_requirement_authority).
narrative_ontology:cs_axiom_status(consensus_requirement_authority, holdable).
narrative_ontology:cs_axiom_grounding('a477be6a-26aa-44cf-a2a7-71ee78236ff5', consensus_requirement_authority, conventional).
narrative_ontology:cs_axiom('a477be6a-26aa-44cf-a2a7-71ee78236ff5', secondary, redistribution_moderation_extraction).
narrative_ontology:cs_axiom_status(redistribution_moderation_extraction, holdable).
narrative_ontology:cs_axiom_grounding('a477be6a-26aa-44cf-a2a7-71ee78236ff5', redistribution_moderation_extraction, instrumental).
narrative_ontology:cs_reference_frame('a477be6a-26aa-44cf-a2a7-71ee78236ff5', stable_descent_group_authority).
narrative_ontology:cs_drift_state('a477be6a-26aa-44cf-a2a7-71ee78236ff5', contemporary_postcolonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a477be6a-26aa-44cf-a2a7-71ee78236ff5', '').
narrative_ontology:cs_kernel_id(customary_rule__lineage_chieftaincy, customary_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(customary_rule__lineage_chieftaincy, recognized_lineages).
narrative_ontology:constraint_beneficiary(customary_rule__lineage_chieftaincy, chiefly_descent_groups).
narrative_ontology:constraint_victim(customary_rule__lineage_chieftaincy, junior_lines).
narrative_ontology:constraint_victim(customary_rule__lineage_chieftaincy, strangers).
narrative_ontology:constraint_victim(customary_rule__lineage_chieftaincy, non_lineage_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRANGER / JUNIOR LINE (SNARE) — No lineage claim and no exit; authority structure is pure extraction with no coordination benefit. Trapped by descent exclusion. Suppression is structural: cannot claim chiefship regardless of competence; cannot appeal outside the lineage council; cannot exit without abandoning land and community. Maximum experienced extraction — the constraint entirely forecloses alternative authority.
constraint_indexing:constraint_classification(customary_rule__lineage_chieftaincy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: JUNIOR LINEAGE SEGMENT (TANGLED ROPE) — Has lineage standing but subordinate seniority. Experiences genuine coordination (consensus-building, collective decision-making, ritual authority) but extraction is real: tribute obligations, labor service, exclusion from senior roles. Constrained by hierarchy and custom but not trapped — succession can elevate; merit can earn council voice. Mixed experience reflects both coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(customary_rule__lineage_chieftaincy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENIOR LINEAGE COUNCIL (ROPE) — Genuine coordination function: council resolves disputes, allocates tribute, maintains ritual calendar, preserves genealogy. Office is older than any holder — succession is regularized, not personal. Extraction exists (tribute, labor) but is structured through redistribution duty and obligatory feast-giving. Mobile exit exists in principle (fission, segment autonomy) even though rarely exercised. Sees the constraint as coordination with embedded redistribution, not as extraction.
constraint_indexing:constraint_classification(customary_rule__lineage_chieftaincy, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: STATE AUTHORITY (TANGLED ROPE) — State recognizes chieftaincy as valid customary institution (coordination function: dispute resolution, land administration, tax collection, ritual legitimacy). But state also extracts value: chiefdom serves as administrative tier, channels tribute upward, suppresses alternative authority claims within state jurisdiction. State sees mixed coordination (uses the institution) and extraction (benefits from institutional legitimacy it gains from recognition). Constrained by need to maintain legitimacy; mobile exit exists (could dissolve chieftaincy, but at cost of institutional collapse).
constraint_indexing:constraint_classification(customary_rule__lineage_chieftaincy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTHROPOLOGICAL / DEVELOPMENT OBSERVER (PITON) — Views chieftaincy as a degraded or vestigial institution: formally recognized (performs ritual function, maintains cultural legitimacy) but functionally displaced by state bureaucracy, market economies, and individualized land tenure. Theater ratio is moderate (ritual performance persists, consensus rhetoric maintained) but actual authority has atrophied or been codified into state-licensed roles. Sees the institution as persisting through inertia and cultural attachment rather than functional necessity. Exit is possible (already occurred in many post-colonial contexts) but the institution survives theatrically.
constraint_indexing:constraint_classification(customary_rule__lineage_chieftaincy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, descent-based authority is presented as immutable: kinship is the irreducible foundation of human social organization; lineage authority emerges naturally from genealogical structure; consensus-based decision-making reflects the immutable coordination problem of kin-group governance. This perspective risks naturalizing what is actually a specific institutional choice — other kinship systems organize authority differently (matriliny vs patriliny, bilateral vs unilineal, age-grade vs lineage councils). The false-summit diagnosis applies: beneficiaries (recognized lineages) have clear structural interest in naturalizing their position.
constraint_indexing:constraint_classification(customary_rule__lineage_chieftaincy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(customary_rule__lineage_chieftaincy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(customary_rule__lineage_chieftaincy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(customary_rule__lineage_chieftaincy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(customary_rule__lineage_chieftaincy, TR),
    TR >= 0.70.

:- end_tests(customary_rule__lineage_chieftaincy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low to moderate. The chief extracts tribute and labor service, but extraction is moderated by redistribution obligation and consensus requirement. The beneficiaries (recognized lineages) are not maximally benefiting because redistribution duties bind them; the victims (strangers and junior lines) are not in maximum extraction because some coordination benefit exists (dispute resolution, ritual coordination, collective defense). The measurement trajectory shows slight rise over 20 units, reflecting increasing theater as state incorporation creates pressure to formalize and perform chieftaincy identity. Suppression (0.52): Moderate-high. Structured suppression is real: descent rule absolutely forecloses outsiders from chiefly authority; seniority rule forecloses junior lines; consensus requirement suppresses dissent or makes dissent socially costly. But suppression is not total — junior lines can advance through succession; outsiders can gain standing through adoption or marriage; consensus can be withdrawn or challenged (though at cost). Theater ratio (0.35): Moderate-low. The constraint exhibits genuine coordination function (consensus-building, dispute resolution, ritual authority) but also genuine extraction (tribute, labor, authority veto). Theater ratio is not low because consensus rituals have real performative content (genealogy recitation, council seating ceremony, feast giving). Theater ratio is not high because the constraint solves real problems (allocating resources, maintaining order) not merely performing legitimacy. The rise in theater over time reflects increasing state pressure to codify and perform 'tradition' as customary identity.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (powerless/trapped stranger) and the rope perspective (institutional/arbitrage senior lineage) are not merely different experiences of the same constraint — they reflect a structural division in who the constraint coordinates and who it extracts from. For senior lineages, chieftaincy solves a real collective-action problem: how to allocate resources, resolve disputes, maintain ritual coordination across a descent group. For strangers and non-members, chieftaincy is pure extraction with no coordination benefit — the constraint forecloses their authority claims. This is not a perspectival gap that disappears with additional information; it is a structural gap revealing that the constraint performs different functions for different agents. The tangled_rope perspective (junior lines) is analytically central: junior lines experience both the coordination function (they benefit from dispute resolution, collective defense) and the extraction (they pay tribute, are excluded from senior authority). The piton perspective reflects institutional decay: as state bureaucracy displaces chiefly authority and market economies displace redistribution obligations, chieftaincy increasingly persists through ritual performance and cultural identity rather than functional necessity. The false-summit perspective reveals the analytical risk: naturalizing descent-based authority as inevitable kinship logic conceals that chieftaincy is a specific institutional choice with clear beneficiaries (recognized lineages) who benefit from naturalizing it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from the agent's structural position: power level, exit options, and whether they benefit or bear costs from the constraint. Recognized lineages are institutional beneficiaries with arbitrage exit (can form new chiefdom through fission, though rarely exercised) — d ≈ 0.15 (low directionality, beneficiary position). Junior lines are moderate power with constrained exit (can advance through succession but succession timing is uncertain) — d ≈ 0.55 (moderate directionality). Strangers are powerless with trapped exit (no kinship claim, cannot exit without abandoning community) — d ≈ 0.95 (high directionality, victim position). The state is institutional with constrained exit (dependent on chieftaincy for administrative legitimacy but can substitute state bureaucracy at cost of institutional collapse) — d ≈ 0.50 (symmetric). The anthropological observer has analytical position with analytical exit (can study or leave) — d ≈ 0.72. Each d value maps through the sigmoid f(d) to produce the agent's experienced effectiveness of extraction (χ). High d values (trapped strangers) produce high f(d) and high χ; low d values (beneficiary lineages) produce low f(d) and potentially negative χ (perceived as benefit rather than extraction).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lineage_descent_closure,
    'Is lineage closure (who counts as descended member) a natural structural feature or a strategic institutional boundary maintained through enforcement?',
    'Historical analysis of admission/exclusion decisions; correlation between closure boundaries and resource scarcity; comparison of closure rules across similar kinship systems; examination of cases where outsiders successfully claimed lineage membership',
    'If natural: validates mountain classification from civilizational view. If strategic: reveals extraction mechanism (junior lines and strangers are excluded by design, not by kinship logic). High impact on victim set definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lineage_descent_closure, empirical, 'Whether lineage closure is natural or strategically maintained').

omega_variable(
    redistribution_duty_enforcement,
    'Is the chief''s redistribution obligation (feast-giving, obligation to support kin, maintenance of commons) actually enforced through exit threat or social sanction? Or is redistribution rhetoric without teeth?',
    'Case studies of chiefs who failed to redistribute: were they removed, sanctioned, or tolerated? Historical records of famine relief obligations; correlation between redistribution failure and political instability; ethnographic accounts of sanctions for chiefly hoarding',
    'If enforced: extraction is moderated, justifies tangled_rope and rope classifications. If unenforced: extraction is higher, shifts classification toward snare and away from rope; suppression is higher (no accountability mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_duty_enforcement, empirical, 'Whether redistribution duties are structurally enforced').

omega_variable(
    consensus_coercion_boundary,
    'Where is the boundary between genuine consensus (inclusive deliberation, reversibility, voluntary assent) and coercive consensus (dissent is socially impossible, decisions reflect power not preference)?',
    'Ethnographic analysis of council meetings: distribution of speaking time, frequency of recorded dissent, consequences of dissent, reversibility of decisions; comparison of decisions reached with demographic preferences (do junior members'' preferences shape outcomes?)',
    'If coercive: consensus is theater; extraction is higher; suppression is higher; classification shifts from rope toward snare. If genuine: coordination function is real; extraction is moderated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_coercion_boundary, empirical, 'Boundary between genuine and coercive consensus').

omega_variable(
    descent_group_fission_exit,
    'Can a lineage segment realistically fission and establish autonomous chieftaincy? Or is fission suppressed by higher-order authority (state, paramount chief, territorial control)?',
    'Historical cases of attempted and successful lineage fission; analysis of fission preconditions (land availability, state enforcement, military capacity); frequency of fission relative to population growth pressure',
    'If fission is suppressed: exit options collapse from mobile/arbitrage toward trapped/constrained; effective suppression rises; classification shifts toward snare. If fission is possible: exit is real; classification stands as tangled_rope/rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(descent_group_fission_exit, empirical, 'Whether lineage fission and segmentary autonomy remain viable exits').

omega_variable(
    reading_contest_land_tenure_coupling,
    'How does THIS reading (lineage chieftaincy as political form) interact with SIBLING READING 1 (customary land tenure as economic form)? Do the readings coexist as distinct institutional layers or does one foreclose the other?',
    'Historical and ethnographic analysis: can land tenure be organized by customary community membership (reading 1) while political authority is organized by lineage chieftaincy (this reading)? Or do the readings require consistent beneficiary and victim sets? Examine cases where readings diverge: e.g., non-lineage members with secure land tenure, or lineage authority without land tenure control.',
    'If readings coexist: both stories remain structurally valid. If one forecloses the other: they must be declared as forecloses/influenced rather than coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_land_tenure_coupling, conceptual, 'Coupling between lineage chieftaincy and customary land tenure readings').

omega_variable(
    reading_contest_elder_adjudication_coupling,
    'How does THIS reading (lineage chieftaincy as political authority) interact with SIBLING READING 2 (elder moot as judicial form)? Do authority structures align (elder moot judges disputes, chiefly council implements remedies) or do they compete (who has legitimacy to resolve disputes)?',
    'Ethnographic analysis: what is the relationship between chiefly council and elder moot? Does the chief chair the moot? Can moot decisions override chiefly authority? How are disputes between the institutions resolved? Compare across societies with both institutions.',
    'If aligned: readings coexist as functional specialization. If competing: one may foreclose the other depending on ethnographic context (some societies privilege elder consensus over chiefly authority; others vice versa).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_elder_adjudication_coupling, conceptual, 'Relationship between chiefly authority and elder adjudication').

omega_variable(
    customary_state_ambiguity,
    'Is ''customary rule'' a stable institutional form or a colonial/post-colonial invention? Was ''chieftaincy'' the pre-colonial political form, or was it reorganized and codified under colonial indirect rule?',
    'Ethnohistorical and archival analysis: comparison of pre-colonial authority structures (if documented) vs colonial-era formalization vs contemporary practice. Examination of ''customary law'' codification: who wrote it down? When? What was excluded? Analysis of cases where customary practices contradict written custom law.',
    'If chieftaincy is pre-colonial: readings are legitimate accounts of stable institutions. If chieftaincy is colonial invention: the constraint instantiates a different extraction mechanism (colonial state using ''tradition'' to legitimize extraction). Shifts false-summit diagnosis and axiom status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(customary_state_ambiguity, empirical, 'Whether chieftaincy is pre-colonial or colonial codification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(customary_rule__lineage_chieftaincy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(customary_chief_tr_t0, customary_rule__lineage_chieftaincy, theater_ratio, 0, 0.28).
narrative_ontology:measurement(customary_chief_tr_t10, customary_rule__lineage_chieftaincy, theater_ratio, 10, 0.35).
narrative_ontology:measurement(customary_chief_tr_t20, customary_rule__lineage_chieftaincy, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(customary_chief_be_t0, customary_rule__lineage_chieftaincy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(customary_chief_be_t10, customary_rule__lineage_chieftaincy, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(customary_chief_be_t20, customary_rule__lineage_chieftaincy, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(customary_chief_su_t0, customary_rule__lineage_chieftaincy, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(customary_chief_su_t10, customary_rule__lineage_chieftaincy, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(customary_chief_su_t20, customary_rule__lineage_chieftaincy, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(customary_rule__lineage_chieftaincy, identity_coordination).
narrative_ontology:affects_constraint(customary_rule__lineage_chieftaincy, customary_rule__land_tenure).
narrative_ontology:affects_constraint(customary_rule__lineage_chieftaincy, customary_rule__elder_adjudication).

% DUAL FORMULATION NOTE:
% Lineage chieftaincy, customary land tenure, and elder adjudication are three readings of the kernel 'customary rule' (kernel_id: customary_rule). They are distinct constraints with different ε values and different beneficiary/victim structures. Each reading focuses on a different institutional dimension (political, economic, judicial) and can exist independently or together in actual societies. The readings interact through network coupling: chieftaincy creates conditions for land tenure constraints (chief allocates land), and both create preconditions for elder adjudication (disputes about chiefly authority and land are adjudicated through moots). But the readings are not identical — land tenure can exist without chiefly authority; moots can operate without lineage-based chieftaincy. Write separate stories for each reading; link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
