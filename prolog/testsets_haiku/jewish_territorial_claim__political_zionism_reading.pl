% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Jewish Territorial Sovereignty and Demographic Majority (Political Zionism Reading)
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   Political Zionism frames Jewish statehood as the structural solution to
 *   the 'Jewish Question'—the supposedly permanent problem of Jewish diaspora
 *   status and vulnerability to antisemitism. This reading prioritizes
 *   territorial sovereignty and demographic majority as prerequisites for
 *   Jewish security and self-determination. It treats Palestinian Arab
 *   inhabitants as obstacles to be managed through displacement, transfer,
 *   legal restriction, or subordination. The constraint's operation—settling
 *   Jewish immigrants, restricting Arab settlement and movement, defining the
 *   state as constitutionally Jewish—directly extracts from Palestinian
 *   communities and redistributes land and political power to Jewish settlers
 *   and Zionist institutions. The claim/metric gap is deliberate: the reading
 *   CLAIMS this as a solution to coordination (resolving diaspora Jewish
 *   insecurity through statehood), and the authored metrics describe high
 *   extraction, substantial suppression, and growing active enforcement—the
 *   engine's computation will measure how far this reading's coordination
 *   story aligns with its actual structural operation.
 *
 * KEY AGENTS:
 *   - Jewish diaspora seeking refuge: powerless globally, trapped in host states, beneficiary from promised statehood and majority protection
 *   - Zionist political leadership: institutional power, sets and enforces the terms of territorial settlement and demographic majority
 *   - Palestinian Arabs in territory: powerless, trapped within claimed territory, structurally victimized by dispossession and subordination
 *   - Bedouin pastoral communities: powerless, trapped within pastoral range, economically undermined by settlement and sovereignty claims
 *   - Arab nationalist leadership: excluded, would contest the entire framing if voice were permitted
 *   - International observer powers: analytical seat, evaluating whether the constraint complies with emerging international norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.82).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.76).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Jewish Territorial Sovereignty and Demographic Majority (Political Zionism Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '21f1f5c4-3320-4af2-a850-7f0bea4c7c38').
narrative_ontology:cs_kernel_codification('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', formalized).
narrative_ontology:cs_authority_grounding('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', extraction).
narrative_ontology:cs_interpretation_layer_present('21f1f5c4-3320-4af2-a850-7f0bea4c7c38').
narrative_ontology:cs_reading_relation('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', foundational, jewish_majority_state_prerequisite_for_diaspora_security).
narrative_ontology:cs_axiom_status(jewish_majority_state_prerequisite_for_diaspora_security, holdable).
narrative_ontology:cs_axiom_grounding('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', jewish_majority_state_prerequisite_for_diaspora_security, empirically_contingent).
narrative_ontology:cs_axiom('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', foundational, palestinian_population_obstacle_to_jewish_statehood).
narrative_ontology:cs_axiom_status(palestinian_population_obstacle_to_jewish_statehood, holdable).
narrative_ontology:cs_axiom_grounding('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', palestinian_population_obstacle_to_jewish_statehood, deontological).
narrative_ontology:cs_reference_frame('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', diaspora_jewish_vulnerability_and_antisemitic_persecution).
narrative_ontology:cs_drift_state('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', late_twentieth_century_human_rights_and_international_law_framework, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('21f1f5c4-3320-4af2-a850-7f0bea4c7c38', '2026-06-11T14:32:18Z').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, european_jewish_intelligentsia).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arabs_in_territory).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, bedouin_pastoral_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities facing persecution, pogroms, and legal discrimination across Europe and the Middle East seek a territorial refuge where they would have majority status and control over their political and legal institutions. They benefit from the constraint insofar as it promises a solution to antisemitism through sovereign statehood — a place where antisemitism would theoretically be impossible because Jews would hold power. Their exit from diaspora conditions is constrained by state citizenship laws, economic barriers, and the sheer scale of migration required.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_seeking_refuge, beneficiary,
    powerless, biographical, trapped, global).

% Sets and administers the political program of securing Palestinian territory for Jewish settlement and statehood. Defines the terms of 'Jewish majority' as a state prerequisite, determines what population transfers or restrictions are necessary to achieve it, and justifies displacement of existing inhabitants as subordinate to the Jewish national project. Controls institutions that coordinate settlement, enforce immigration restrictions, and manage political discourse around the constraint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Intellectuals, journalists, and political organizers who articulate the ideological justification for territorial sovereignty and the necessity of Jewish demographic majority. They benefit by gaining voice, institutional platform, and political power through the Zionist movement — translating diaspora marginalization into state-building leadership. They can exit by withdrawing from the movement, but doing so costs them influence and communal standing.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, european_jewish_intelligentsia, beneficiary,
    organized, generational, mobile, global).

% Indigenous agricultural and urban communities with several centuries of established settlement patterns, property rights, and political organization. Under the political Zionism reading, they are reframed as obstacles to Jewish majority status — the constraint operates directly to dispossess them of land, restrict their movement and settlement rights, and subordinate their political claims to Jewish statehood requirements. Their 'exit' would require displacement or forced assimilation; remaining means accepting permanent minority status in a state constitutionally defined as Jewish.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arabs_in_territory, payer,
    powerless, generational, trapped, regional).

% Nomadic and semi-nomadic pastoralists whose territorial range spans the claimed territory. Territorial sovereignty with fixed Jewish settlement patterns directly undermines their pastoral economy. The constraint forces them to either settle (abandoning traditional livelihood), migrate out of the region, or face legal restrictions on movement. They have no institutional voice in defining the terms of Jewish majority.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, bedouin_pastoral_communities, payer,
    powerless, immediate, trapped, regional).

% The existing sovereign authority over Palestinian territory under the constraint's emergence. The Ottoman state is displaced by the Zionist project — the constraint operates to transfer sovereignty from Ottoman imperial law to Zionist state law. From this seat, the constraint is a direct challenge to state authority and territorial integrity.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, ottoman_imperial_authority, observer,
    institutional, biographical, analytical, continental).

% Emerging Arab nationalist movements would articulate competing territorial and majority claims if admitted to the governance conversation. Their exclusion is maintained by the constraint's enforcement machinery — they are structurally barred from negotiating the terms of 'Jewish majority' or the status of Arab populations. They would contest the entire framing of the constraint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, arab_nationalist_leadership, excluded,
    organized, generational, trapped, continental).

% European powers and later international bodies monitor and evaluate whether Jewish statehood claims are viable under international law. They examine whether the constraint complies with principles of self-determination and minority rights, and whether territorial sovereignty is achievable without violating standing international norms. Their observations feed into the contestation of the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_observer_powers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the diaspora Jewish coordination problem: uniting scattered Jewish communities under a single sovereign framework where they hold majority power and control institutions. Without territorial sovereignty and demographic majority, diaspora Jews remain politically and legally subordinate to host states; the constraint coordinates them into a unitary political body where antisemitism would be structurally impossible (as Jews would hold power).
% TRANSFER_FUNCTION: Transfers land, property, and political sovereignty from Palestinian Arab inhabitants and Ottoman imperial authority to Jewish settlers and a Zionist state. Moves Palestinian communities from majority inhabitants with property rights and local governance to minority status (or displacement) within a state defined as Jewish. Moves international legitimacy from Ottoman/Arab nationalist frameworks to Zionist state frameworks.
% ABSENT_VOICES: Palestinian Arab communities and their nationalist leadership are structurally excluded from the conversation that defines 'Jewish majority' and its prerequisites. Arab pastoralists and small-holders have no institutional voice in determining what displacement or restriction is 'necessary.' International lawyers and observers outside the Zionist movement would contest whether territorial transfer and demographic engineering comply with emerging norms of self-determination and minority protection.
% DISAPPEARANCE_RATIONALE: If the constraint—the requirement for Jewish territorial sovereignty with demographic majority—vanished overnight, Palestinian Arab communities would remain majority inhabitants, Ottoman sovereignty (or Arab nationalist governance) would persist or expand, and diaspora Jewish communities would continue seeking refuge through immigration and cultural-nationalist frameworks instead of state-building. The entire structure of Israel as a Jewish state would not exist; the region would reorganize under different sovereignty and demographic assumptions.
% FOUNDING_PROBLEM: Antisemitism is endemic and unsolvable in the diaspora. Jewish communities face perpetual legal discrimination, violence, and exclusion across Europe and the Middle East. The only structural solution is a territory where Jews hold majority power and can create institutions that protect Jewish collective interests—a Jewish state. Without statehood and majority status, Jews remain vulnerable to host-state persecution.
% FOUNDING_PROBLEM_CORROBORATION: Zionist political leadership attests the founding problem is live and permanent. Some diaspora Jewish communities in crisis (fleeing pogroms, responding to Dreyfus crisis) attest to the urgency of refuge. However, non-Zionist Jewish leaders, international observers, and historians contest whether statehood with demographic majority is the only or best solution to antisemitism—pointing instead to legal reform, diaspora cultural nationalism, international human-rights frameworks, or integration. The corroboration is DIVIDED: the founding problem (antisemitism) is attested from many seats, but the causal claim (that it requires territorial majority statehood) is contested by significant constituencies outside the benefiting political leadership.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 to 0.82 across the interval, tracking the escalation of settlement intensity, land acquisition, and legal restrictions on Palestinian property and movement. At t=0 (early Zionist political organization) the extraction exists but is lower because settlement is still limited in scale and Palestinian Arab resistance is not yet forcefully organized. By t=20 (post-WWI British Mandate) extractiveness climbs sharply as settlement accelerates and land-purchase restrictions intensify. By t=50 (late 1940s statehood and war) extraction is at maximum because territorial transfer is fait accompli and suppression of Arab political claims is total. Suppression requirement follows a similar trajectory: at t=0, Palestinian Arab resistance is localized and unorganized; by t=50, sustained suppression of Arab nationalism, refugee repatriation claims, and residual Palestinian communities inside the state requires continuous institutional effort. Theater ratio remains moderate (0.28–0.41) because the settlement enterprise does involve genuine ideological and organizational work—it is not pure performance—but the gap between the rhetoric of Jewish security and the actual mechanism (land dispossession, population transfer) grows across the interval. At t=50, theater has risen because the founding problem (diaspora antisemitism) has been partly decoupled from the constraint's actual operation (maintaining Jewish state demographic majority and Arab subordination), making the security justification increasingly theatrical relative to the structural maintenance function. All measurements authored on a single shared time grid; all metrics present at every t.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute radically different types from structural data alone. From the Zionist political leadership and diaspora Jewish seats, the constraint appears as genuine rope (coordination of diaspora Jews into a protective sovereign body). From Palestinian Arab seats, it appears as pure snare or worse (enforced dispossession with no coordination benefit, only subordination). The engine computes these per-seat types from power, exit, directionality, and the authored beneficiary/victim structure; the authored claim (tangled_rope) sits between these poles. A tangled_rope requires BOTH coordination (the beneficiary part: solving diaspora Jewish insecurity) AND extraction (the victim part: Palestinian dispossession), both sustained by the same structure. This is exactly what political Zionism instantiates: it genuinely coordinates diaspora Jewish interests into a sovereign state, AND it genuinely extracts from Palestinian Arabs through land seizure and legal subordination. The same institutions (state administration, settlement planning, military) that create Jewish majority protection simultaneously dispossess Palestinians. This is not rope viewed from different angles—it is structural asymmetry, properly captured as tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora refugees sit at low directionality (d ≈ 0.2–0.3): they benefit substantially from the promise of majority-status refuge and Jewish-controlled institutions. They bear some costs (labor migration, displacement from diaspora homes, military participation) but the net flow is toward them. Zionist political leadership sits at very low directionality (d ≈ 0.05–0.15): they control the constraint's definition and enforcement, collect institutional power and legitimacy, face minimal costs. Palestinian Arabs and Bedouins sit at very high directionality (d ≈ 0.85–0.95): the constraint's operation directly dispossesses them, restricts their freedom of movement and property ownership, subordinates them to a state they did not create and cannot democratically control. The asymmetry is extreme: one seat's benefit is another seat's extraction. This is the structural signature of a tangled rope with massive per-seat divergence—genuine coordination for the beneficiary seats (uniting diaspora Jews under sovereign protection), pure coercion for the victim seats (forced displacement and subordination). International observer powers sit at moderate directionality (d ≈ 0.5): they gain legitimacy and practical relevance from evaluating the constraint's legality, but they also bear costs if their validation enables human-rights violations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora antisemitism) is contested as to its permanence and solution. By t=50, a significant body of evidence and analysis (historical documents, international law development, non-Zionist Jewish voices, Palestinian historical records) contests whether the founding problem actually required territorial majority statehood OR whether it could have been solved through legal reform, diaspora cultural nationalism, and international human-rights frameworks. This contestation is not trivial: it goes to the core justification for the constraint. A constraint whose founding problem is dead or solved but which persists for other reasons (territorial expansion, institutional inertia, power capture) would normally trigger mandatrophy. However, political Zionism's claim is that the founding problem is not dead—that diaspora antisemitism remains a live threat even after the Holocaust, and that a Jewish state with majority protection remains necessary. This is itself contested (some argue WWII lessons point toward international legal frameworks, others toward strengthened diaspora communities). The mandate has not fully atrophied because the threat narratives remain live, even as they shift. A piton diagnosis would require the founding problem to be clearly dead AND the constraint to persist through mere institutional inertia; here the constraint persists partly through renewed threat narratives and partly through institutional capture of state resources. This is tangled_rope with disputed mandate, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_permanence,
    'Is antisemitism in the diaspora a permanent, structural condition that cannot be solved by legal reform and international law, or is it a contingent historical phenomenon that could be addressed through other mechanisms?',
    'Post-WWII evolution of international human-rights frameworks, Holocaust impact on diaspora legal status, and empirical change in antisemitic violence over subsequent decades. Historical analysis of whether diaspora Jewish communities achieved security and acceptance without territorial majority statehood (as post-war diaspora communities did).',
    'If antisemitism is contingent and solvable by legal/international reform, the founding problem is overestimated and the constraint''s justification shifts from solving a permanent problem to pursuing territorial ambitions. If antisemitism is permanent, the constraint''s mandate remains live and the coordination function (gathering diaspora Jews into majority-protected statehood) is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_permanence, empirical, 'Whether the founding problem (diaspora antisemitism) is permanent or contingent on historical conditions.').

omega_variable(
    territorial_solution_necessity,
    'Is territorial sovereignty with demographic majority the ONLY viable structural solution to Jewish insecurity, or are alternative solutions (legal citizenship protections, cultural nationalism, diaspora institution-building, international law) structurally equivalent?',
    'Comparative analysis of post-WWII diaspora Jewish communities and their legal/political security outcomes. Assessment of whether non-territorial solutions achieved the stated goal (protecting Jews from persecution and giving them voice in self-governance).',
    'If territorial majority is necessary, political Zionism''s claim is vindicated and the constraint is a genuine coordination response to an irreducible problem. If alternatives exist and work, the constraint becomes one choice among many—and the extraction from Palestinians becomes harder to justify as a necessary cost rather than an optional territorial acquisition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_solution_necessity, conceptual, 'Whether territorial majority statehood is the only structurally viable solution or one option among alternatives.').

omega_variable(
    palestinian_displacement_necessity,
    'Is population displacement or legal subordination of Palestinian Arabs a necessary consequence of achieving Jewish demographic majority, or could Jewish majority statehood coexist with Palestinian political and property rights?',
    'Demographic and historical analysis of whether Jewish majority could be achieved through migration alone without land seizure from existing inhabitants. Assessment of whether political rights for Palestinian minorities are compatible with a constitutionally Jewish state.',
    'If displacement is necessary, the extraction from Palestinians is a structural cost of the coordination solution—tangled rope with unavoidable asymmetry. If displacement is optional (achievable through voluntary migration on available land), the constraint becomes a choice to prioritize Jewish majority over Palestinian rights—collapsing from tangled rope into snare (pure extraction rationalized as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palestinian_displacement_necessity, conceptual, 'Whether Palestinian displacement is structurally necessary or a contingent policy choice.').

omega_variable(
    jewish_majority_permanence,
    'Is the requirement for permanent Jewish demographic majority a structural feature of Jewish security (because minorities are inherently vulnerable), or is it a territorial-maximalist claim layered onto the security frame?',
    'Examination of whether Jewish minorities in diaspora were actually less secure than Jewish majorities in the state. Assessment of whether the requirement for 60%/70%/80% Jewish population is justified by security data or is driven by maximalist settlement ideology.',
    'If majority is structurally necessary for security, political Zionism''s claim stands. If majority is a maximalist ideological commitment, the constraint shifts toward revisionist Zionism and the justification becomes territorial expansion rather than Jewish protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_majority_permanence, empirical, 'Whether Jewish demographic majority is a security requirement or a territorial-maximalist commitment.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can political Zionism and Palestinian Arab nationalism coexist as live frameworks held by different parties, or does political Zionism''s core logic (Jewish state with demographic majority) logically foreclose Palestinian statehood and self-determination?',
    'Logical and political analysis of whether Jewish-majority-state requirements in the same territory leave room for Palestinian self-determination, or whether they necessarily foreclose it.',
    'If they logically foreclose each other, the reading_relations should be ''forecloses'' rather than ''coexists_with''. If they can theoretically coexist with different territorial arrangements or power-sharing, ''coexists_with'' is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether this reading''s core claims logically foreclose Palestinian nationalism or whether coexistence is theoretically possible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__political_zionism_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(jewi_tr_t10, jewish_territorial_claim__political_zionism_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__political_zionism_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(jewi_tr_t30, jewish_territorial_claim__political_zionism_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__political_zionism_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(jewi_tr_t50, jewish_territorial_claim__political_zionism_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(jewi_be_t10, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(jewi_be_t30, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(jewi_be_t50, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(jewi_su_t10, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(jewi_su_t30, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(jewi_su_t50, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__political_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, palestinian_national_claim__arab_nationalism_reading).

% DUAL FORMULATION NOTE:
% The jewish_territorial_claim kernel is contested across four Zionist movements (cultural, labor, political, revisionist) and is incompletely specified—different readings assign different requirements to 'Jewish territorial claim'. This story instantiates the political Zionism reading: state-building with Jewish demographic majority, treating Palestinian Arabs as obstacles requiring management. Sibling readings emphasize cultural nationalism (without necessarily requiring majority), socialist labor movement (transformation through settlement), and revisionist maximalism (both banks of Jordan, Iron Wall). Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different per-seat type computations. Links via affects_constraints document the constraint family and the empirical fact that changes in one reading's justification or implementation create structural pressure on the others (e.g., if Palestinian displacement becomes politically untenable, labor Zionism's settlement facts-on-ground strategy faces new constraints, and cultural Zionism gains legitimacy as a less extractive alternative). Palestinian nationalism is upstream: its emergence and claims directly constrain and challenge the political Zionism reading's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, powerless, 0.88).
constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, organized, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
