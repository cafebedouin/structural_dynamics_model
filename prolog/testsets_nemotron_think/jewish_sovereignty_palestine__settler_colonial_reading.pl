% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Zionist Settlement Displacement Regime (Settler-Colonial Reading)
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint story instantiates the settler_colonial_reading of the
 *   jewish_sovereignty_palestine kernel. It reads Zionism as a European
 *   settler-colonial project in which Jewish immigration — regardless of the
 *   refugees' motives or persecution — functions as a displacement regime
 *   that structurally eliminates Palestinian presence and sovereignty. The
 *   colonial metropole shifts from British mandatory power (1917-1948) to
 *   U.S. imperial interests (post-1967, especially post-1973), but the
 *   zero-sum territorial logic persists: the constraint extracts land,
 *   sovereignty, and demographic dominance from Palestinians for the benefit
 *   of the metropole-settler alliance. The claimed type is snare: the
 *   coordination story (Jewish self-determination, refugee haven) is cover;
 *   persistence depends on active suppression of Palestinian alternatives
 *   (return, equal citizenship, independent statehood) and on the metropole's
 *   strategic sponsorship.
 *
 * KEY AGENTS:
 *   - palestinians: Primary target (powerless/trapped) — bears structural dispossession, demographic engineering, military rule
 *   - jewish_immigrants: Settler-beneficiary (moderate/constrained) — receives land, state privileges, but serves as instrument of metropole
 *   - british_imperial_interests: Initial metropole beneficiary/agenda_setter (institutional/arbitrage) — used Zionist settlement as imperial wedge, withdrew when costs exceeded benefits
 *   - us_imperial_interests: Successor metropole beneficiary (institutional/arbitrage) — provides diplomatic, military, economic cover for the displacement regime
 *   - international_legal_order: Observer (institutional/analytical) — produces resolutions and opinions the constraint treats as advisory
 *   - arab_states: Excluded (organized/constrained) — would object to displacement but are structurally sidelined by metropole power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.85).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Zionist Settlement Displacement Regime (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '3dcf4ed2-012e-4827-92d1-a8787e92c188').
narrative_ontology:cs_kernel_codification('3dcf4ed2-012e-4827-92d1-a8787e92c188', distributed).
narrative_ontology:cs_authority_grounding('3dcf4ed2-012e-4827-92d1-a8787e92c188', extraction).
narrative_ontology:cs_reading_relation('3dcf4ed2-012e-4827-92d1-a8787e92c188', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('3dcf4ed2-012e-4827-92d1-a8787e92c188', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3dcf4ed2-012e-4827-92d1-a8787e92c188', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3dcf4ed2-012e-4827-92d1-a8787e92c188', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('3dcf4ed2-012e-4827-92d1-a8787e92c188', foundational, settler_colonialism_structurally_incompatible_with_indigenous_sovereignty).
narrative_ontology:cs_axiom_status(settler_colonialism_structurally_incompatible_with_indigenous_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3dcf4ed2-012e-4827-92d1-a8787e92c188', settler_colonialism_structurally_incompatible_with_indigenous_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('3dcf4ed2-012e-4827-92d1-a8787e92c188', foundational, jewish_immigration_functions_as_settler_colonial_mechanism_regardless_of_motive).
narrative_ontology:cs_axiom_status(jewish_immigration_functions_as_settler_colonial_mechanism_regardless_of_motive, holdable).
narrative_ontology:cs_axiom_grounding('3dcf4ed2-012e-4827-92d1-a8787e92c188', jewish_immigration_functions_as_settler_colonial_mechanism_regardless_of_motive, empirically_contingent).
narrative_ontology:cs_reference_frame('3dcf4ed2-012e-4827-92d1-a8787e92c188', settler_colonial_structural_analysis).
narrative_ontology:cs_drift_state('3dcf4ed2-012e-4827-92d1-a8787e92c188', post_oslo_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3dcf4ed2-012e-4827-92d1-a8787e92c188', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_interests).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, settler_colonialism_structurally_incompatible_with_indigenous_sovereignty).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, demographic_engineering_as_displacement_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to military rule, permit regimes, land expropriation, settlement expansion, and denial of return. The constraint extracts their land, sovereignty, and demographic presence. No exit preserves their political community — leaving is exile, staying is subjugation. Resistance is continuous but met with overwhelming suppression.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinians, payer,
    powerless, generational, trapped, regional).

% Receive land, citizenship, state subsidies, and dominant legal status through the displacement regime. Their refugee/persecution history is real but structurally irrelevant to their position as settlers — the constraint operates regardless of intent. They bear costs (military service, insecurity, moral injury) but these are the costs of maintaining the beneficiary position, not extraction from them. Exit (emigration) means surrendering the privileges the constraint grants.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants, payer).

% Issued the Balfour Declaration and managed the Mandate to use Zionist settlement as a imperial wedge against French influence and Arab nationalism. Extracted strategic value (Suez access, air routes, oil pipeline security) while offloading governance costs onto the settler population. Withdrew in 1948 when the constraint's maintenance costs exceeded its imperial utility — clean exit available.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests, agenda_setter,
    institutional, generational, arbitrage, regional).

% After 1967 (especially post-1973), became the primary metropole sponsor: provides $3B+ annual military aid, UN veto cover, diplomatic normalization pressure on Arab states, and strategic integration (intelligence, tech, basing). Extracts a reliable regional partner that enforces U.S. interests without U.S. boots on ground. Exit is available but strategically costly — the constraint is a high-value asset, not a burden.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_interests, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_interests, agenda_setter).

% Produces resolutions (194, 242, 338, 2334), ICJ opinions, ICC investigations that the constraint treats as advisory. The legal framework (self-determination, prohibition of acquisition of territory by force, apartheid crime) structurally contradicts the constraint's operation but lacks enforcement capacity against the metropole. Its situation is to document the violation while the violation continues.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_legal_order, observer,
    institutional, generational, analytical, universal).

% Would object to the displacement regime and have fought wars (1948, 1967, 1973) to reverse it. Structurally excluded from the constraint's decision-making by metropole power and their own dependence on U.S. security/economic ties. Normalization agreements (Camp David, Oslo, Abraham Accords) demonstrate their constrained exit: they can protest but cannot effectively resist the metropole-settler structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, arab_states, excluded,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_interests).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the settlement of a foreign population onto inhabited land, allocating land, water, legal rights, and state resources to the settlers while managing the metropole's strategic interests. The coordination is real: Hebrew revival, institutional building, defense coordination, and economic integration among Jewish immigrants are genuine collective-action solutions.
% TRANSFER_FUNCTION: Moves land, water, sovereignty, demographic weight, and legal rights from Palestinians to Jewish immigrants and the metropole. The metropole receives strategic control (bases, intelligence, regional influence) and arms-market revenue. Jewish immigrants receive statehood, land, and dominant legal status. Palestinians lose all three.
% ABSENT_VOICES: Palestinian refugees in diaspora (denied return, denied voice in the regime that displaces them); Mizrahi Jews (Arab Jews whose migration was engineered by the same metropole-settler structure but who are racialized within it); anti-Zionist Jewish voices (excluded from the 'Jewish consensus' the constraint performs). They are absent because the constraint's enforcement machinery (citizenship laws, entry permits, definition of 'Jewish state') structurally silences them.
% DISAPPEARANCE_RATIONALE: If the displacement regime vanished overnight, the land/settlement architecture would face immediate contestation: Palestinian return claims, settlement legal status, Jerusalem sovereignty, water rights, and the metropole's strategic footprint would all require renegotiation. The Jewish-Israeli polity would lose its demographic-engineered majority and its legal framework of privilege. The metropole would lose its primary regional anchor. The world rearranges fundamentally.
% FOUNDING_PROBLEM: The liberal_nationalist_reading claims the founding problem was Jewish persecution and statelessness in Europe (pogroms, Holocaust, closed borders). This reading acknowledges the persecution but argues the chosen solution — a settler-colonial displacement regime in Palestine — was not the only or necessary solution, and that the constraint now generates the very statelessness it claims to solve (Palestinian refugees).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Jewish persecution) is corroborated by universal historical record. The claim that Zionist settlement in Palestine was the necessary/only solution is corroborated only by the benefiting parties (Zionist movement, metropole sponsors). Palestinian historians, British Mandate archives (e.g., Palin Commission, Hope Simpson Report), and contemporaneous anti-colonial Jewish voices (Bundists, Communists, Mizrahi opponents) attest that alternatives existed (immigration to US/UK, binational state proposals, refugee resettlement elsewhere) and were structurally blocked by the metropole-settler alliance.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.85) reflects zero-sum territorial logic: every dunam of land, every demographic percentage point, every unit of sovereignty is a direct transfer from Palestinians to the settler-metropole structure. Suppression (0.9) is near-total: the constraint maintains itself through military occupation, permit regimes, settlement expansion, legal exclusion of return, and metropole veto at the UN. Theater (0.4) acknowledges real state-building and coordination among Jewish immigrants (Hebrew revival, institutions, defense) but treats these as the coordination layer that makes the extraction efficient, not as an independent justification. Accessibility collapse (0.8) is high because the Oslo process collapsed the alternative of independent statehood into a fragmented autonomy that deepened the displacement regime. Resistance (0.85) is sustained across the interval: armed struggle, intifadas, BDS, legal challenges, sumud.
 *
 * PERSPECTIVAL GAP:
 *   From the liberal_nationalist_reading seat, the same facts appear as legitimate national liberation with unfortunate but resolvable conflict. From the religious_zionist_reading seat, the constraint is divine fulfillment, not extraction. From the post_zionist_reading seat, the constraint is a historical phase now obstructing civic equality. The engine computes these divergences from the structural data; this story authors only the settler_colonial_reading's structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinians are full targets (d → 1.0): trapped by geography, identity-locked to land, no exit that preserves political community. Jewish immigrants are net beneficiaries (d → 0.2) but with constrained exit (they cannot exit the conflict without abandoning the privileges the constraint grants them). British and U.S. imperial interests are full beneficiaries (d → 0.0): they collect strategic value (regional foothold, intelligence, arms markets) while bearing minimal direct cost. The metropole shift does not alter the Palestinians' structural position — the extraction architecture is continuous.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (per liberal_nationalist_reading: Jewish persecution and statelessness) is real but the arrangement that claims to solve it has structurally become a displacement regime that generates new statelessness (Palestinian refugees). The constraint persists not because the founding problem is solved but because the metropole-settler alliance extracts ongoing value from the unsolved conflict. This is mandatrophy: the mandate (Jewish safety) has atrophied into a structure that requires Palestinian unfreedom to maintain Jewish-Israeli privilege.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the settler_colonial_reading of the contested kernel jewish_sovereignty_palestine. How does the structural classification change when the same historical phenomena are read through liberal_nationalist, religious_zionist, cultural_zionist, or post_zionist readings?',
    'Author each sibling reading as a separate constraint story with its own ε, stakeholders, and claimed_type; compare engine-computed seat classifications across the family.',
    'If sibling readings produce divergent classifications for the same agent seats (e.g., jewish_immigrants as beneficiary here vs. liberated_national_subject in liberal_nationalist), the kernel is structurally fractured — no single constraint captures the phenomenon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment of this file to one reading of a multi-reading kernel; structural delta across readings').

omega_variable(
    refugee_status_vs_settler_function,
    'Does the refugee/persecuted status of Jewish immigrants (pre-1948 especially) structurally modify their position as settlers in the displacement regime, or is the settler function analytically prior to motive?',
    'Compare constraint stories that treat motive as structurally relevant (would produce different ε for pre- vs post-1948) vs. those that treat structural position as motive-independent (this reading).',
    'If motive modifies structure, this reading''s ε overstates extraction for early periods; if motive is irrelevant to structural position, the reading''s high ε holds across the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_status_vs_settler_function, conceptual, 'Whether immigrant motive alters the constraint''s structural classification').

omega_variable(
    metropole_shift_britain_to_us,
    'Is the shift from British to U.S. imperial interest as primary metropole beneficiary a continuous structural relationship or a discrete substitution that changes the constraint''s character?',
    'Measure extraction and suppression metrics across the 1948/1967 transitions; test whether the engine computes a type transition at the metropole shift.',
    'If continuous, single constraint story suffices; if discrete, the interval should be split into two stories linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_shift_britain_to_us, empirical, 'Whether metropole succession constitutes one constraint or two').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_scr_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jsp_scr_tr_t1936, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1936, 0.25).
narrative_ontology:measurement(jsp_scr_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(jsp_scr_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.38).
narrative_ontology:measurement(jsp_scr_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(jsp_scr_tr_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jsp_scr_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement(jsp_scr_be_t1936, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1936, 0.6).
narrative_ontology:measurement(jsp_scr_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement(jsp_scr_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.82).
narrative_ontology:measurement(jsp_scr_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.83).
narrative_ontology:measurement(jsp_scr_be_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jsp_scr_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.55).
narrative_ontology:measurement(jsp_scr_su_t1936, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1936, 0.75).
narrative_ontology:measurement(jsp_scr_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.88).
narrative_ontology:measurement(jsp_scr_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(jsp_scr_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.88).
narrative_ontology:measurement(jsp_scr_su_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__settler_colonial_reading, 0.18).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'Zionism' into structurally distinct claims. The settler_colonial_reading (this story) and liberal_nationalist_reading have ε values that differ by >0.5 — they are not the same constraint viewed from two angles. The ε-invariance principle requires separate stories linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__settler_colonial_reading, moderate, 0.25).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__settler_colonial_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
