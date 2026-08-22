% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Jewish Statehood via Territorial Sovereignty and Jewish Majority (Political Zionism)
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   Political Zionism, as one reading of the contested
 *   jewish_territorial_claim kernel, frames the solution to antisemitism and
 *   the Jewish Question as requiring territorial sovereignty in Palestine
 *   with a Jewish demographic majority. This reading prioritizes
 *   state-building and institutional power over cultural or ideological
 *   content. Palestinian Arabs are structurally positioned as an obstacle to
 *   the majority prerequisite — their presence must be reduced through
 *   settlement, land acquisition, and ultimately displacement or
 *   subordination. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination function for diaspora Jews + active enforcement mechanism)
 *   while the measurement series describe substantial extractiveness and high
 *   suppression — the engine will compute whether the claim holds or the
 *   metrics indicate a snare. The claim/metric divergence is intentional and
 *   diagnostic.
 *
 * KEY AGENTS:
 *   - European Jewish communities: diaspora victims of antisemitism, seeking refuge and security through statehood
 *   - Zionist leadership: agenda-setter defining the problem (antisemitism as unsolvable under minority status) and solution (majority Jewish state); controls interpretation and organizational strategy
 *   - Palestinian Arab population: powerless payer bearing land dispossession, political subordination, and displacement
 *   - Ottoman and British colonial authorities: enforce the constraint through legal, administrative, and military apparatus
 *   - International powers (League of Nations, Western states): legitimize the constraint through legal frameworks and military backing
 *   - Other Zionist currents: excluded or marginalized alternatives (cultural, labor, revisionist) that emphasize different aspects of the Jewish territorial claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.68).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.72).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Jewish Statehood via Territorial Sovereignty and Jewish Majority (Political Zionism)").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '3b32b90f-3d41-4dba-af86-364078c33b95').
narrative_ontology:cs_kernel_codification('3b32b90f-3d41-4dba-af86-364078c33b95', fixed_text).
narrative_ontology:cs_authority_grounding('3b32b90f-3d41-4dba-af86-364078c33b95', extraction).
narrative_ontology:cs_interpretation_layer_present('3b32b90f-3d41-4dba-af86-364078c33b95').
narrative_ontology:cs_reading_relation('3b32b90f-3d41-4dba-af86-364078c33b95', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('3b32b90f-3d41-4dba-af86-364078c33b95', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('3b32b90f-3d41-4dba-af86-364078c33b95', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('3b32b90f-3d41-4dba-af86-364078c33b95', foundational, diaspora_jewish_status_inherently_unsolvable).
narrative_ontology:cs_axiom_status(diaspora_jewish_status_inherently_unsolvable, holdable).
narrative_ontology:cs_axiom_grounding('3b32b90f-3d41-4dba-af86-364078c33b95', diaspora_jewish_status_inherently_unsolvable, empirically_contingent).
narrative_ontology:cs_axiom('3b32b90f-3d41-4dba-af86-364078c33b95', foundational, majority_jewish_state_prerequisite_for_jewish_security).
narrative_ontology:cs_axiom_status(majority_jewish_state_prerequisite_for_jewish_security, holdable).
narrative_ontology:cs_axiom_grounding('3b32b90f-3d41-4dba-af86-364078c33b95', majority_jewish_state_prerequisite_for_jewish_security, empirically_contingent).
narrative_ontology:cs_reference_frame('3b32b90f-3d41-4dba-af86-364078c33b95', jewish_diaspora_vulnerability_unsolvable_without_statehood).
narrative_ontology:cs_drift_state('3b32b90f-3d41-4dba-af86-364078c33b95', contemporary_post_colonial_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b32b90f-3d41-4dba-af86-364078c33b95', '2026-06-19T00:00:00Z').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, european_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_leadership).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, ottoman_authorities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, british_mandate_administration).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_question_solvable_through_statehood).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, antisemitism_remediable_by_territorial_separation).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, majority_jewish_state_prerequisite_for_jewish_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Jews facing persecution, pogroms, and systemic discrimination in Eastern Europe, antisemitic legal restrictions in Western Europe, and vulnerability as stateless minorities. The constraint offers them a solution narrative: statehood in Palestine as refuge and security. Materially, some benefit from emigration and settlement (land acquisition, state institutions, security); others experience emigration as costly and dangerous. The constraint's benefit to this seat is the legitimacy and hope of the solution, and actual refuge for those who emigrate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, european_jewish_communities, beneficiary,
    moderate, generational, mobile, global).

% Central authority defining the problem (Jewish Question = unsolvability of diaspora status), the solution (majority Jewish state in Palestine), and the strategy (settlement, land acquisition, institutional consolidation). Includes the Zionist Organization leadership, settlement agencies, and political intellectuals. Controls the interpretation of Jewish security needs, the legitimacy of territorial claims, and resource allocation from diaspora communities. Extracts organizational power, ideological authority, and material resources.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_leadership, agenda_setter,
    organized, generational, arbitrage, global).

% Existing residents and landowners of Palestine who experience land dispossession through settlement and legal mechanisms, population displacement through violence and expulsion, political marginalization and subordination to Jewish institutions, and exclusion from self-determination. They have no seat at the table where the problem or solution is defined. Their presence is treated as an obstacle to the majority prerequisite rather than as a people with claims. They cannot exit (nowhere to go, forcibly prevented) and cannot defeat the constraint (militarily and politically overpowered by organized Zionist movement backed by colonial authorities and international powers).
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Ottoman imperial authority permits and regulates Jewish settlement in Palestine under millet system (religious community autonomy). Benefits by maintaining control over the territory while extracting deference from both Jewish settlers and Arab residents. Enforces the constraint through land law, immigration regulation, and security apparatus. Ottoman decline after WWI removes this seat, replaced by British Mandate authority.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, ottoman_authorities, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, ottoman_authorities, beneficiary).

% League of Nations Mandate authority (1920-1948) administers Palestine and enforces the constraint through legal frameworks (recognizing Jewish settlement, facilitating land acquisition), military force (suppressing Arab resistance), and diplomatic legitimation (Balfour Declaration, mandate terms). Benefits by managing regional strategic value, extracting deference from both parties, and maintaining imperial presence. Directly enforces suppression of Palestinian Arab political alternatives and resistance.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandate_administration, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandate_administration, beneficiary).

% Legitimize the constraint through international legal recognition (Balfour Declaration, League of Nations Mandate, UN Partition Plan, UN recognition of Israel). Provide military and economic support to Israel. Maintain the structural conditions (international law, military deterrence, diplomatic backing) that enforce Jewish majority and Palestinian subordination. Their seats are analytical and institutional, not materially extractive in the same way as Palestinian Arabs.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_powers_western_states, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, international_powers_western_states, agenda_setter).

% Alternative Zionist reading emphasizing socialist transformation and social regeneration through labor ('conquest of labor') rather than pure state-building. Would argue for different settlement strategies (kibbutzim, collective ownership, worker focus) and different relationships with Palestinian Arab workers. Partially excluded from agenda-setting because political Zionism's emphasis on state and majority crowds out the labor reading's social priorities. Some labor Zionists institutionalize (kibbutzim, Histadrut) but within the political-Zionist state framework, not as independent alternative.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, labor_zionist_movement, excluded,
    moderate, generational, constrained, regional).

% Jewish intellectuals and thinkers (Ahad Ha'am, Martin Buber, others) who advocate for Jewish spiritual and cultural center in Palestine without necessarily requiring political sovereignty or demographic majority. Would argue that the Jewish Question is solvable through cultural renaissance rather than state-building. Marginalized in the political-Zionist institutional dominance; their influence is primarily cultural and intellectual, not institutional.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, cultural_zionist_intellectuals, excluded,
    powerless, biographical, constrained, global).

% More militant Zionist current (Jabotinsky, Begin, others) that contests political Zionism by claiming maximalist territorial boundaries (both banks of Jordan) and advocating immediate military sovereignty rather than gradualism. Would argue that political Zionism is too accommodating to colonial authorities and Arab resistance. Partially excluded from dominance during Mandate era, but gains influence post-1948 through military movements (Irgun, Lehi) and later political parties. Represents more extractive and militaristic reading of the same kernel.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, revisionist_zionist_movement, excluded,
    organized, generational, constrained, regional).

% Jewish communities and intellectuals (especially Eastern European Bundists, assimilationists, Orthodox anti-Zionists, later postcolonial theorists) who reject Zionism or advocate alternative solutions to the Jewish Question (assimilation, equal rights, cultural autonomy in diaspora, communism). Their objections are systematically suppressed through institutional capture of diaspora Jewish representation by Zionist organizations, narrative dominance, and resource control. Identity-locked: they are Jewish, so their exclusion from the conversation is highly salient; yet they are foreclosed from shaping the Jewish response to antisemitism.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, anti_zionist_and_non_zionist_jewish_communities, excluded,
    powerless, biographical, identity_locked, global).

% Palestinian Arab and broader Arab nationalist movements that contest Jewish settlement and demand Palestinian statehood and self-determination. Structurally excluded from the agenda-setting by the constraint — their political alternatives (Palestinian state, binational arrangement, negotiated settlement with Arab plurality) are foreclosed by the majority prerequisite. Their resistance is met with state and international force. Their claims are suppressed through military dominance and international backing of the Jewish state.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, arab_nationalist_movement, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, zionist_leadership).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scattered diaspora Jewish communities around a unified solution to antisemitism and vulnerability — statehood as the collective Jewish answer. Solves the coordination problem of directing multiple Jewish communities toward a shared project of settlement, institutional building, and state formation. Creates shared political identity and organizational structure.
% TRANSFER_FUNCTION: Transfers land and territorial control from Palestinian Arab residents and owners to Jewish settlers and the Jewish state. Transfers political sovereignty and state power to Jewish institutions from Ottoman and British colonial authorities (and later to Israeli state as independent actor). Transfers legitimacy and international recognition to Jewish statehood from Western powers and international institutions. Transfers hope and organizational commitment from diaspora Jewish communities to the Zionist movement and state institutions.
% ABSENT_VOICES: Palestinian Arabs have no voice in defining the problem (antisemitism) or the solution (majority state). Their territorial and political claims are never entertained as legitimate alternatives. Anti-Zionist and non-Zionist Jewish voices are suppressed through institutional capture of diaspora representation by Zionist organizations. Labor and cultural Zionist readings are marginalized by political Zionism's institutional dominance within the movement. Arab nationalist and broader regional powers are excluded from negotiating Palestinian status and territorial arrangement.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if political Zionism's definition of the solution (majority Jewish state) were abandoned — the entire institutional, legal, and territorial arrangement of Israel and the Palestinian territories would require reorganization. The state would lose its founding justification. Land tenure, citizenship, and sovereignty claims would be redistributed. Palestinian political claims would move from suppressed to structurally possible. The entire regional strategic balance would shift. The world would rearrange because the constraint has become the constitutive framework for the region's political order.
% FOUNDING_PROBLEM: Diaspora antisemitism, Jewish vulnerability and persecution in Christian and Muslim lands, the 'Jewish Question' — the perceived insolubility of Jewish minority status, rights, and security under non-Jewish rule. Antisemitic violence, legal restrictions, economic marginalization, and existential threat to diaspora communities.
% FOUNDING_PROBLEM_CORROBORATION: Zionist and Jewish communal leadership attest that the founding problem remains live and ongoing antisemitism validates the solution. Historians and Jewish scholars document antisemitic persecution in 19th-20th century Europe as empirical fact. However, postcolonial scholars, critical historians, and historians of antisemitism attest that: (1) the 'Jewish Question' as formulated by Zionism was a constructed problem, not inherent to diaspora life; (2) alternative solutions existed and were pursued (assimilation in Germany and France, equal rights movements, socialist internationalism, cultural autonomy); (3) European persecution was contingent on specific political-economic conditions (industrialization, nation-state consolidation, economic competition), not an eternal feature of minority status; (4) the solution (territorial statehood with majority) was not inevitable, but one political choice among alternatives; (5) the framing of alternatives as impossible was itself a Zionist ideological project, not a structural inevitability.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers land and political sovereignty from Palestinian Arabs to Jewish settlement and statehood — a zero-sum transfer with no compensation mechanism. The measurement series shows extractiveness rising from 0.35 (early ideological phase) to 0.68 (established statehood) as the constraint moves from aspiration to institutional fact. Suppression is higher still (0.72) because Palestinian Arab resistance to displacement is met with state coercion; alternatives to the constraint (binational state, Palestinian autonomy, negotiated settlement) are structurally foreclosed by the majority prerequisite. Theater ratio rises from 0.25 to 0.41 as the constraint shifts from active settlement/displacement phase to maintenance of established state — an increasing proportion of enforcement effort goes to defending the legitimacy narrative ('security state', 'Jewish homeland') rather than accomplishing the actual work of territorial consolidation. All measurements use a shared time grid (t=0 through t=100) so every metric is authored at identical time points; the interval represents roughly 1897 (First Zionist Congress) to 1948-1997 (statehood through consolidation).
 *
 * PERSPECTIVAL GAP:
 *   The claim (tangled_rope: real coordination + asymmetric extraction) versus the metrics (extractiveness 0.68, suppression 0.72, theater 0.41) diverges because tangled_rope presupposes that the coordination function is genuine and necessary — that the two sides benefit from the same structure even as one extracts from the other. The measurement profile suggests that by the constraint's mature phase (t=60-100), the extractive function increasingly dominates and the coordination narrative increasingly becomes theater (theater_ratio rising from 0.25 to 0.41). If the engine computes a snare classification from these metrics despite the claimed tangled_rope, the divergence indicates either: (a) the extraction outweighs the coordination by the end state, or (b) the coordination is real but achieves it through such asymmetric means that it collapses into pure extraction. The mandatrophy analysis below addresses this.
 *
 * DIRECTIONALITY LOGIC:
 *   The zionist_leadership seat has d near 0.2 (full beneficiary): sets and enforces the constraint, collects the coordination gains (organizational power, legitimacy, resources), benefits from the solution (statehood achieved). The european_jewish_communities seat has d near 0.35 (beneficiary with costs): benefits from refuge and security, but bears immigration costs, violence risk, and opportunity costs of leaving diaspora. The palestinian_arab_population seat has d near 0.95 (full target): dispossessed of land, politically subordinated, excluded from self-determination, no meaningful exit. The colonial_authority seats have d near 0.45 (moderate extractor): extract deference and strategic value from managing both parties, but also bear administrative costs and military burden. No directionality overrides are needed — the structural derivation from beneficiary/victim + power + exit_options is coherent.
 *
 * MANDATROPHY ANALYSIS:
 *   Political Zionism's founding mandate is to solve the Jewish Question through territorial statehood with Jewish majority. By the constraint's mature phase (t=60-100), this mandate is ACHIEVED — the state is established, Jews hold the territory, majority is secured. Yet the constraint does not dissolve or transition to a post-founding form. Instead, extractiveness plateaus at 0.68 and suppression at 0.72, suggesting the constraint has transformed into a maintenance mechanism rather than a mandate-executing instrument. The theater ratio rising from 0.25 to 0.41 indicates increasing proportion of enforcement effort devoted to defending the legitimacy narrative ('security state', 'Jewish homeland', 'necessary majority') rather than accomplishing the actual work of state-building — a classic zombie constraint signature. The divergence between achieved mandate (statehood + majority) and persistent extraction (land control, political subordination, suppression of alternatives) suggests mandatrophy is LIVE: the founding problem (diaspora antisemitism, vulnerability) is solved for the beneficiary seats (Jewish statehood achieved), but the constraint persists because it now serves extractive functions unrelated to its founding — territorial accumulation, Palestinian subordination, regional military dominance — that have become ends in themselves. A reading-specific omega (id: reading_specificity_and_sibling_foreclosure) addresses whether this transformation is inherent to political Zionism or a contingent drift from the original reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jewish_question_solvability,
    'Is the ''Jewish Question'' (diaspora vulnerability and antisemitism) inherently unsolvable without territorial sovereignty and Jewish demographic majority, or are alternative solutions (assimilation, equal rights, autonomy) structurally viable?',
    'Historical counterfactual analysis: comparison of outcomes for diaspora Jewish communities that pursued assimilation, equal rights, and cultural autonomy versus those that pursued territorial statehood, controlling for antisemitic pressure and state capacity.',
    'If alternative solutions were viable, the claim that statehood is necessary (and justified by necessity) collapses — the constraint becomes pure preference-driven territorial nationalism rather than remedial. If statehood was necessary, the extraction from Palestinian Arabs may be reclassified as tragic necessity rather than pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jewish_question_solvability, conceptual, 'Whether statehood was the only solution to the Jewish Question or one option among viable alternatives.').

omega_variable(
    palestinian_agency_and_consent,
    'To what extent can the Palestinian Arab population''s dispossession and subordination be characterized as an inevitable byproduct of Jewish state-building versus a structurally chosen mechanism of the political-Zionist reading?',
    'Archival analysis of political-Zionist leadership discourse and strategy documents; comparison of alternative institutional arrangements (binational state, federation, separate statehood with negotiated transfer) that were considered and rejected; examination of whether Palestinian political alternatives were foreclosed by choice or structural necessity.',
    'If dispossession was structurally chosen rather than inevitable, the constraint shifts from tangled_rope (coordination + tragic extraction) to snare (pure extraction covered by coordination narrative). If inevitable, the extraction measure itself becomes ambiguous — necessary cost vs. exploitative suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_agency_and_consent, empirical, 'Whether Palestinian dispossession was inevitable or chosen as the political-Zionist strategy.').

omega_variable(
    reading_specificity_and_sibling_foreclosure,
    'Does political Zionism''s emphasis on state-building and Jewish majority FORECLOSE labor Zionism''s socialist regeneration reading, or do the readings COEXIST as different emphases within the same kernel?',
    'Genealogy of the Zionist movement: examine whether political Zionists actively suppressed labor-Zionist alternatives, whether labor Zionists maintained independent theoretical space, whether the two readings shaped institutional forms (kibbutzim, histadrut) that represent genuine coexistence or political hierarchy.',
    'If reading forecloses sibling, the kernel is intrinsically contested and political Zionism''s dominance is a contingent victory of one reading over another. If readings coexist, political Zionism''s claim to represent THE solution to the Jewish Question is weaker — it is one among multiple Zionist strategies. The classification of the constraint as tangled_rope depends on whether the ''coordination'' is genuine or cover for a reading-specific extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specificity_and_sibling_foreclosure, empirical, 'Whether political Zionism forecloses or coexists with labor and cultural Zionist readings.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is Palestinian Arab resistance suppression primarily structural (legal exclusion, military force, land tenure law) or partially internalized (accepted inevitability, identity loss, normalization of subordination)?',
    'Post-conflict scenario analysis: if the suppressive mechanisms (military, legal) were removed, would Palestinian resistance persist at current levels or would it amplify? Examination of Palestinian political consciousness and collective identity — is subordination perceived as temporary constraint or absorbed into identity?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (0.72) suggests — targets carry the constraint''s logic with them into any post-conflict scenario. If structural, removal of enforcement machinery would rapidly decompress resistance. This affects the piton/zombie classification: an internalized suppression suggests the constraint has become partially cultural, harder to reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether Palestinian suppression is structural or internalized.').

omega_variable(
    kernel_reading_decomposition,
    'Is this constraint one coherent reading of the jewish_territorial_claim kernel, or does political Zionism itself decompose into distinct sub-readings (state-building vs. security, gradual vs. military, territorial extent)?',
    'Definitional genealogy: trace the evolution of political Zionism''s central claims from Herzl forward; identify moments where different emphases (state as refuge vs. state as engine of cultural regeneration; gradualism vs. militarism; Balfour-line borders vs. maximalist claims) competed; determine whether unified political movement or coalition of readings.',
    'If political Zionism decomposes, this story should split into multiple constraint files per the ε-invariance principle. If unified, the single story suffices. The kernel contest (cultural, labor, political, revisionist) is unambiguous — this story is political Zionism. But internal coherence affects the reliability of the beneficiary/victim and extraction measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether political Zionism is one coherent reading or itself a coalition of sub-readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__political_zionism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t0, projected).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__political_zionism_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__political_zionism_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t40, observed).
narrative_ontology:measurement(jewi_tr_t60, jewish_territorial_claim__political_zionism_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t60, observed).
narrative_ontology:measurement(jewi_tr_t80, jewish_territorial_claim__political_zionism_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t80, observed).
narrative_ontology:measurement(jewi_tr_t100, jewish_territorial_claim__political_zionism_reading, theater_ratio, 100, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(jewi_be_t0, projected).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(jewi_be_t40, observed).
narrative_ontology:measurement(jewi_be_t60, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(jewi_be_t60, observed).
narrative_ontology:measurement(jewi_be_t80, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(jewi_be_t80, observed).
narrative_ontology:measurement(jewi_be_t100, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(jewi_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(jewi_su_t0, projected).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement_basis(jewi_su_t40, observed).
narrative_ontology:measurement(jewi_su_t60, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(jewi_su_t60, observed).
narrative_ontology:measurement(jewi_su_t80, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement_basis(jewi_su_t80, observed).
narrative_ontology:measurement(jewi_su_t100, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement_basis(jewi_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__political_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, palestinian_self_determination_constraint).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, british_mandate_administration_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the jewish_territorial_claim kernel. Each reading produces a distinct constraint with different ε (extractiveness), beneficiary/victim structure, and classification. Political Zionism emphasizes state-building and Jewish majority as prerequisites; it thus produces higher extractiveness and suppression than cultural or labor readings, which emphasize cultural/social content. Revisionist Zionism produces even higher extractiveness. The four readings COEXIST as live positions in historical Zionism; political Zionism's dominance is a contingent institutional victory, not a logical foreclosure. Each reading's story is linked via network.affects_constraints to the others — they are constraint-family siblings instantiating different readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
