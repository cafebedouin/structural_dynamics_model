% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__cultural_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Jewish Cultural Renaissance in Palestine (Cultural Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates the cultural Zionist reading of the
 *   contested kernel 'jewish_sovereignty_palestine'. Under this reading, the
 *   primary value is Jewish cultural renaissance and revitalization of Hebrew
 *   language—a secular, intellectual, and artistic regeneration—achieved
 *   through concentration of diaspora Jewish talent in Palestinian
 *   institutions. Critically, this reading does NOT require political
 *   sovereignty, demographic majority, or zero-sum displacement of
 *   Palestinian Arab inhabitants. Beneficiaries are Hebrew language
 *   development, Jewish cultural institutions, and diaspora intellectuals
 *   seeking a secular center of Jewish meaning-making. The constraint is
 *   presented as a coordination mechanism solving diaspora fragmentation, not
 *   as an extraction mechanism requiring political dominion. However, the
 *   reading remains contested: sibling readings (settler-colonial,
 *   liberal-nationalist, religious Zionist, post-Zionist) reframe the same
 *   historical facts and institutions as instantiating different constraints
 *   with vastly different extractiveness and suppression profiles.
 *
 * KEY AGENTS:
 *   - secular_jewish_nationalist_faction: Cultural Zionist intellectuals (Ahad Ha'am circle, Bialik) articulating the reading; organized, mobile exit, sets agenda for cultural development
 *   - jewish_cultural_institutions: Hebrew theaters, universities, literary journals, artistic communities; moderate power, generational time horizon, beneficiary of concentration
 *   - jewish_intellectual_diaspora: European, American, Middle Eastern Jewish intellectuals, artists, seekers; organized, arbitrage-grade exit (can maintain diaspora citizenship), primary beneficiary
 *   - palestinian_arab_inhabitants: Analytically present as co-inhabitants under this reading but structurally excluded from the conversation setting terms; organized, constrained exit, observer seat
 *   - ottoman_and_mandate_authorities: Formal governance structures with power to permit/restrict; excluded, trapped exit (territorial jurisdiction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.28).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.15).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Jewish Cultural Renaissance in Palestine (Cultural Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, '555e33b7-9b67-47b7-a6af-af8eaed250b4').
narrative_ontology:cs_kernel_codification('555e33b7-9b67-47b7-a6af-af8eaed250b4', distributed).
narrative_ontology:cs_authority_grounding('555e33b7-9b67-47b7-a6af-af8eaed250b4', diffuse_epistemic).
narrative_ontology:cs_reading_relation('555e33b7-9b67-47b7-a6af-af8eaed250b4', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('555e33b7-9b67-47b7-a6af-af8eaed250b4', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('555e33b7-9b67-47b7-a6af-af8eaed250b4', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('555e33b7-9b67-47b7-a6af-af8eaed250b4', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('555e33b7-9b67-47b7-a6af-af8eaed250b4', foundational, secular_jewish_cultural_vitality_sufficient).
narrative_ontology:cs_axiom_status(secular_jewish_cultural_vitality_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('555e33b7-9b67-47b7-a6af-af8eaed250b4', secular_jewish_cultural_vitality_sufficient, instrumental).
narrative_ontology:cs_axiom('555e33b7-9b67-47b7-a6af-af8eaed250b4', foundational, jewish_presence_compatible_palestinian_autonomy).
narrative_ontology:cs_axiom_status(jewish_presence_compatible_palestinian_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('555e33b7-9b67-47b7-a6af-af8eaed250b4', jewish_presence_compatible_palestinian_autonomy, deontological).
narrative_ontology:cs_reference_frame('555e33b7-9b67-47b7-a6af-af8eaed250b4', diaspora_jewish_cultural_fragmentation).
narrative_ontology:cs_drift_state('555e33b7-9b67-47b7-a6af-af8eaed250b4', contemporary_post_statehood, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('555e33b7-9b67-47b7-a6af-af8eaed250b4', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_language_revitalization).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_intellectual_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ahad Ha'am, Bialik, and cultural Zionists articulate the reading and set the agenda for cultural development priorities, institutional architecture, and the framing of Jewish presence as culturally revitalizing rather than politically expansionist. They defend Hebrew language development as the primary Jewish achievement, not statehood. They can exit to diaspora if the arrangement becomes unsustainable, but maintain generational commitment to the project.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, secular_jewish_nationalist_faction, agenda_setter,
    organized, generational, mobile, regional).

% Hebrew-language theaters, universities, literary journals, art academies, and philosophical circles flourishing in Palestine. They benefit from the concentration of diaspora talent and resources. They depend on cultural vitality rather than political control for their legitimacy and can theoretically relocate if institutional conditions change, though they claim unique value from Palestinian geographical and historical connection.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_institutions, beneficiary,
    moderate, generational, mobile, global).

% European, American, and Middle Eastern Jewish intellectuals, artists, writers, philosophers, and spiritual seekers who migrate to or collaborate with Palestinian cultural institutions. They receive concentrated access to Hebrew language immersion, Jewish artistic creation, and secular Jewish meaning-making. They maintain diaspora citizenship, professional networks, and publishing venues, so exit is feasible (arbitrage-grade), but they invest biographical labor and cultural capital in the project.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_intellectual_diaspora, beneficiary,
    organized, biographical, arbitrage, global).

% Under this reading, Palestinian inhabitants are understood as co-inhabiting the same cultural and spiritual space as Jewish institutions. They are not displaced by the cultural renaissance itself. However, they are analytically EXCLUDED from the conversation that frames and sets the cultural agenda—they are present but not speaking. Their constrained exit reflects that they cannot easily leave Palestine; they are organized politically but outside the institutional structure that defines this particular constraint. They would have substantial objections to their observer status if present in the conversation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants, observer,
    organized, generational, constrained, regional).

% Ottoman and British Mandate governance structures with territorial jurisdiction and authority to regulate cultural institutions. They are excluded because the cultural Zionist reading frames Jewish institutional development as transcending formal governance—cultural authority is presented as anterior to or independent of state authority. The authorities experience this as loss of control over institutional licensing and cultural regulation. Their trapped exit reflects that they cannot abandon territorial jurisdiction; they are forced to manage a dynamic (cultural institutional growth) they did not authorize and cannot easily suppress without seeming oppressive.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, ottoman_and_mandate_authorities, excluded,
    institutional, biographical, trapped, regional).

% Rabbinical authorities and Orthodox Jewish institutions that ground legitimacy in divine law and theological interpretation. Under this reading they are relegated to secondary status—Hebrew language and secular Jewish culture are presented as the primary Jewish achievement, not Torah study or Halakhic development. They would dispute that cultural secular vitality constitutes authentic Jewish purpose without theological grounding. They have constrained exit because their authority is institutional and portable, but their claims to legitimate interpretation of Judaism are directly challenged by the secular framing.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, religious_authority_structures, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_institutions).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__cultural_zionist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assembles dispersed Jewish intellectuals, artists, and spiritual seekers into a concentrated cultural renewal movement in Palestine. Solves the coordination problem of diaspora Jewish cultural fragmentation and the revival of Hebrew as a living secular language by creating institutional infrastructure (universities, theaters, literary circles, academies) that concentrates diaspora talent and resources. Coordinates diaspora patronage, intellectual labor, and artistic energy toward a common cultural project without requiring political control or state apparatus.
% TRANSFER_FUNCTION: Moves intellectual labor, patronage, and cultural capital from diaspora Jewish communities (Eastern European, Western European, American, Middle Eastern Jewish centers) toward Palestinian cultural institutions. Channels diaspora financial support, writers, artists, and scholars into Hebrew-language cultural production. The transfer flows primarily in one direction: from diaspora resources to Palestinian institutional development of secular Jewish culture.
% ABSENT_VOICES: Palestinian Arab intellectuals and political leaders are structurally absent from the conversation that frames the cultural arrangement, though acknowledged as present inhabitants. Under this reading they are not victims of displacement but neither are they speakers who define the terms of cultural coexistence. They would insist on: (1) joint governance of cultural policy reflecting Arabic-language culture as equally primary; (2) explicit guarantees against political subordination or demographic pressure; (3) reciprocal resource flows supporting Palestinian intellectual institutions; (4) recognition that concentrating diaspora resources in Jewish institutions may constitute a form of cultural-political pressure even without physical displacement. Religious Zionists and Orthodox authorities are also excluded; they would argue that secular cultural vitality cannot ground Jewish legitimacy without theological foundation and divine promise.
% DISAPPEARANCE_RATIONALE: If the cultural institutional complex (Hebrew theaters, universities, literary journals, academies) disappeared overnight, Hebrew language development would decelerate significantly, diaspora Jewish intellectuals would disperse to multiple sites (Berlin, New York, Vienna) rather than concentrating in Palestine, and secular Jewish cultural meaning-making would fragment. However, the disappearance would NOT necessarily erase Palestinian autonomy or require demographic reorganization, because under this reading the cultural arrangement does not depend on territorial expansion or displacement. Palestinian inhabitants would retain their land and political options. The difference from other readings: the settler-colonial reading predicts that losing the cultural center would unmask underlying displacement; the liberal-nationalist reading predicts that cultural institutions are covers for statehood claims; this reading predicts that cultural coordination would simply disperse without geopolitical rearrangement.
% FOUNDING_PROBLEM: Jewish people in the diaspora are culturally fragmented, atomized, and at risk of assimilation into gentile societies. Hebrew language is moribund, used only for liturgy. Jewish intellectual, artistic, and spiritual life lacks a coherent center or institutional home. Jewish secular identity—meaning-making that is Jewish without being primarily religious or nationalist—lacks institutional expression and social space.
% FOUNDING_PROBLEM_CORROBORATION: Ahad Ha'am, Bialik, and cultural Zionist intellectuals attest the founding problem as empirically real in early 20th-century Eastern European Jewish communities. Contemporary diaspora Jewish intellectuals (US, European, Middle Eastern) affirm the problem's historical reality. However, Palestinian intellectuals and post-Zionist critics dispute whether the founding problem justifies the territorial choice: they argue that Hebrew language could be revitalized through diaspora pluralism, that secular Jewish identity could flourish without geographic concentration, and that the founding problem invokes a Western European anxiety (assimilation) that is not universal to Jewish experience. They cite Yiddish cultural revival, American Jewish institutional development, and diaspora Zionism as alternative solutions to the same problem. European historians confirm the cultural ferment in early 20th-century Eastern Europe but dispute causation and necessity. Religious Zionists reject the premise that secular cultural vitality solves the authentic founding problem—they argue the real founding problem is spiritual exile from the land and its solution requires theological restoration, not cultural academies.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28 plateau from t=20 onward) because under this reading's own lights, Jewish cultural presence does not require dispossession—Palestinians retain their own cultural and political autonomy within the same space. The reading explicitly disavows zero-sum demographic competition or political subordination. Suppression is also LOW (0.15) because the cultural institutions operate through intellectual and artistic persuasion, not coercive exclusion (though formal governance structures exert background force). Theater ratio is MINIMAL (0.12) because the cultural work is genuine—Hebrew literature, theater, philosophy—not theatrical performance covering extraction. Accessibility of alternatives remains moderate (0.35) because the cultural achievement could theoretically be replicated elsewhere or through different organizational forms, though the reading asserts that geographic concentration in Palestine has unique spiritual and historical significance. Resistance is non-trivial (0.42) because Palestinian intellectuals and post-Zionist critics actively dispute the reading's framing, arguing either that cultural presence inevitably entails political subordination or that the framing obscures displacement occurring under other readings. The measurement trajectory shows extraction rising slightly in the early interval (t=0 to t=20) as the institutional complex solidifies and resource flows increase, then plateauing—this reflects the reading's claim that once the cultural center is established, the extractiveness stabilizes because no further displacement is required. The claim/metric independence is deliberate: this reading is CLAIMED as pure coordination (rope) while metrics capture modest but non-zero extractiveness (the cost of managing the excluded voices and the suppression required to prevent displacement narratives from surfacing). The engine will compute per-seat classifications; the settler-colonial reading sitting in the same constraint family will author vastly different metrics on the same historical facts.
 *
 * PERSPECTIVAL GAP:
 *   Different seats experience radically different constraint types. From the cultural Zionist agenda-setter's seat, this is genuine coordination—solving the real problem of diaspora fragmentation. From Palestinian intellectuals' seat, the same institutions constitute a form of cultural-political pressure that subordinates Arabic as primary or forces Palestinian participation in a Jewish-framed meaning-space. From the settler-colonial reading, the cultural institutions are covers for demographic expansion and territorial claim-staking. From the religious Zionist seat, secular cultural vitality is a false foundation—the authentic Jewish claim rests on divine promise, not artistic achievement. The engine computes these per-seat divergences; this reading author is committing to a specific structural picture (low extractiveness, coordination-based, non-zero-sum) while remaining neutral on whether that picture is empirically or morally true.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are explicitly named: jewish_cultural_institutions (moderate power, mobile exit—d low, near beneficiary); hebrew_language_revitalization (non-agent, treated as vindicated proposition); jewish_intellectual_diaspora (organized, arbitrage-grade exit—d low to symmetric, they contribute labor but can exit to diaspora citizenship). Victims array is EMPTY under this reading—the cultural Zionist framing explicitly denies that Jewish presence requires Palestinian victimization. This is the core structural difference from sibling readings. Palestinian_arab_inhabitants are named as observer stakeholders with constrained exit, which situates them as potentially affected but not as victims under THIS reading. Ottoman/mandate authorities are excluded (trapped exit, institutional power—they experience loss of control). The directionality derivation should reflect these declarations: beneficiaries with mobile/arbitrage exit should compute d in the 0.0–0.3 range; the excluded seats should show structural pressure (higher d) because they bear the suppression cost of preventing displacement narratives. No directionality override is needed if the base declarations work correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora cultural fragmentation, Hebrew language revival) remains CONTESTED in status, which is accurate: diaspora intellectuals affirm it as live; post-Zionist critics argue it was partially solved through other means (Yiddish culture, diaspora institutions, assimilation as an alternative). The disappearance verdict is WORLD_REARRANGES because institutional concentration in Palestine is presented as solving a real coordination problem—disperse the institutions and the coordination benefit is lost. However, the reading's claim that disappearance would NOT erase Palestinian autonomy reflects the non-zero-sum framing. The mandatrophy question is whether this reading's founding problem (secular Jewish cultural vitality) genuinely requires the territorial site or whether it could be pursued through diaspora pluralism. This ambiguity is captured in the omegas rather than resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_presence_and_political_pressure,
    'Does institutional concentration of Jewish cultural resources in Palestine constitute a form of political-cultural pressure on Palestinian autonomy even absent physical displacement?',
    'Empirical observation: Palestinian intellectual and political responses to the cultural institutions; whether Palestinians experience the cultural arrangement as neutral coordination or as subordinating their own cultural meanings to a Jewish-framed default. Counterfactual: what would Palestinian autonomy look like if it were genuine—would it include equivalent institutional resources for Arabic-language culture and Palestinian intellectual life?',
    'If the cultural arrangement does constitute subordinating pressure, extractiveness should be reclassified upward toward tangled_rope (coordination + asymmetric extraction). If Palestinian cultural resources are genuinely equivalent and autonomy is respected, the rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_presence_and_political_pressure, empirical, 'Whether cultural concentration without displacement still constitutes extractive pressure on meaning-making authority.').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Do the cultural Zionist axioms logically foreclose the settler-colonial or religious Zionist readings, or do they coexist as incompatible positions held by different parties?',
    'Logical analysis: can a single institutional arrangement be experienced as non-zero-sum secular cultural coordination AND as divine territorial fulfillment or demographic settlement? The answer determines whether this reading forecloses others (logically impossible in one framework) or coexists (empirically held simultaneously by different actors).',
    'If forecloses, the engine''s relationship_classification should mark foreclosed edges in the kernel family. If coexists, the readings remain live options for different parties despite mutual incompatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, conceptual, 'Whether this reading''s core premises logically eliminate or permit coexistence with sibling readings.').

omega_variable(
    diaspora_resource_transfer_asymmetry,
    'Does the flow of diaspora Jewish intellectual and financial capital into Palestinian institutions constitute a net transfer that benefits only Jewish cultural development, or does it generate Palestinian institutional capacity?',
    'Economic analysis: trace capital flows, examine whether Palestinian institutions benefit from diaspora support, whether Palestinians gain research or artistic capacity, whether the transfer is one-directional or reciprocal.',
    'If one-directional extraction from diaspora to Jewish institutions only, extractiveness rises toward snare territory. If genuinely reciprocal institutional strengthening, the rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_resource_transfer_asymmetry, empirical, 'Whether diaspora capital transfer serves mutual institutional development or asymmetric Jewish enrichment.').

omega_variable(
    secular_cultural_vitality_as_reading_ground,
    'Is secular Jewish cultural achievement (literature, theater, language revitalization) a sufficient ground for collective Jewish presence in Palestine, or does this reading depend on unstated territorial or religious claims?',
    'Textual analysis: examine whether cultural Zionist arguments rest solely on cultural merit or invoke historical/theological connection to land. If cultural achievement alone were the ground, could the same institutions flourish elsewhere with equal legitimacy?',
    'If secular cultural merit is truly independent of territory, the reading stands as non-zero-sum. If territory is smuggled in as a hidden premise, the reading merges with liberal nationalist or religious Zionist claims and extractiveness should rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_cultural_vitality_as_reading_ground, conceptual, 'Whether the cultural Zionist reading''s legitimacy is independent of territorial claims or covertly depends on them.').

omega_variable(
    mandatrophy_temporal_rupture,
    'At what point does this reading''s founding problem (diaspora cultural fragmentation, Hebrew language revival) become functionally obsolete—when does the cultural center become self-perpetuating independent of the original problem it solved?',
    'Historical observation: after the cultural institutions are established and Hebrew becomes a living language, are they maintained because the original problem persists, or because institutional inertia and identity-fusion make them self-perpetuating regardless of the founding problem''s status?',
    'If institutional self-perpetuation uncouples from the founding problem, the constraint drifts toward piton (performance-heavy maintenance of a function that no longer runs). If the founding problem remains live, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_temporal_rupture, empirical, 'Whether the cultural institutions transition from solving a real coordination problem to maintaining themselves through performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t5, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement_basis(jewi_tr_t5, observed).
narrative_ontology:measurement(jewi_tr_t10, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(jewi_tr_t10, observed).
narrative_ontology:measurement(jewi_tr_t15, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t15, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t25, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t25, observed).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t30, observed).
narrative_ontology:measurement(jewi_tr_t40, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t5, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement_basis(jewi_be_t5, observed).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(jewi_be_t10, observed).
narrative_ontology:measurement(jewi_be_t15, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement_basis(jewi_be_t15, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t25, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(jewi_be_t25, observed).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(jewi_be_t30, observed).
narrative_ontology:measurement(jewi_be_t40, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(jewi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t5, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement_basis(jewi_su_t5, observed).
narrative_ontology:measurement(jewi_su_t10, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(jewi_su_t10, observed).
narrative_ontology:measurement(jewi_su_t15, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 15, 0.13).
narrative_ontology:measurement_basis(jewi_su_t15, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t25, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(jewi_su_t25, observed).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement_basis(jewi_su_t30, observed).
narrative_ontology:measurement(jewi_su_t40, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(jewi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__cultural_zionist_reading, 0.1).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel jewish_sovereignty_palestine. The five sibling readings share the same historical facts and institutions but frame them with different beneficiary/victim structures, authority grounds, and extractiveness profiles. The cultural Zionist reading instantiates low extractiveness and non-zero-sum framing. The settler-colonial and liberal-nationalist readings on the same institutional facts author high extractiveness and asymmetric victim/beneficiary distributions. These are not measurement disagreements—they are different constraints with different structural cores derived from reading the kernel through different epistemological and normative frameworks. Each is compiled as a separate constraint story; the network edges enable cross-kernel analysis of how readings affect each other's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
