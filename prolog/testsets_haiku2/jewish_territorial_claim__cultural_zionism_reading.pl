% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Jewish Cultural and Spiritual Center in Palestine (Cultural Zionism Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The cultural zionism reading frames Jewish territorial claim to Palestine
 *   as grounded in cultural and spiritual renaissance—Hebrew language
 *   revival, Jewish institutional autonomy, and creative flourishing—without
 *   requiring political sovereignty, demographic majority, or Arab
 *   displacement. This reading instantiates one strand of early zionist
 *   thought that emphasized cultural regeneration and educational
 *   institutions over state-building. The constraint establishes Jewish
 *   cultural centers, schools, theaters, agricultural communities, and
 *   spiritual institutions in Palestine as coordinating mechanisms for
 *   cultural autonomy. Within this reading, Arab presence is not inherently
 *   threatening; the framework remains open to binational coexistence where
 *   Arab and Jewish communities maintain separate institutional spheres.
 *   However, the materiality of settlement—land acquisition, institutional
 *   dominance, demographic change—imposes asymmetric costs on Arab
 *   Palestinian communities regardless of the reading's stated intent. The
 *   measured extractiveness reflects this gap: the reading's normative frame
 *   emphasizes cultural benefit and coexistence potential, but the structural
 *   operation transfers resources and administrative control. Theater ratio
 *   rises and falls as the performative element of 'non-political' cultural
 *   settlement waxes and wanes with political events.
 *
 * KEY AGENTS:
 *   - Jewish cultural practitioners: establish and sustain Hebrew-language institutions, educational centers, spiritual communities; benefit from territorial autonomy for cultural development
 *   - Arab Palestinian residents: experience land transfer, institutional marginalization, demographic pressure; constrained exit due to economic ties and political fragmentation
 *   - Ottoman/Mandate authorities: set and enforce legal framework; shift role as administrative regime changes
 *   - Hebrew language revivalists: subset of cultural practitioners with focus on linguistic revival as living language medium
 *   - Arab intellectual and political elites: excluded from the reading's joint-framework vision; would contest separation of culture from politics
 *   - Binational framework advocates: analytical observers tracking whether cultural autonomy remains separable from sovereignty claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.38).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.22).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Jewish Cultural and Spiritual Center in Palestine (Cultural Zionism Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/nationalism/settler_colonialism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '228d83a6-4c00-45aa-b0ff-034531947045').
narrative_ontology:cs_kernel_codification('228d83a6-4c00-45aa-b0ff-034531947045', distributed).
narrative_ontology:cs_authority_grounding('228d83a6-4c00-45aa-b0ff-034531947045', lineage).
narrative_ontology:cs_interpretation_layer_present('228d83a6-4c00-45aa-b0ff-034531947045').
narrative_ontology:cs_reading_relation('228d83a6-4c00-45aa-b0ff-034531947045', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('228d83a6-4c00-45aa-b0ff-034531947045', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('228d83a6-4c00-45aa-b0ff-034531947045', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_axiom('228d83a6-4c00-45aa-b0ff-034531947045', foundational, cultural_autonomy_separable_from_sovereignty).
narrative_ontology:cs_axiom_status(cultural_autonomy_separable_from_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('228d83a6-4c00-45aa-b0ff-034531947045', cultural_autonomy_separable_from_sovereignty, deontological).
narrative_ontology:cs_axiom('228d83a6-4c00-45aa-b0ff-034531947045', foundational, binational_coexistence_structurally_possible).
narrative_ontology:cs_axiom_status(binational_coexistence_structurally_possible, holdable).
narrative_ontology:cs_axiom_grounding('228d83a6-4c00-45aa-b0ff-034531947045', binational_coexistence_structurally_possible, empirically_contingent).
narrative_ontology:cs_axiom('228d83a6-4c00-45aa-b0ff-034531947045', secondary, hebrew_revival_requires_territorial_anchor).
narrative_ontology:cs_axiom_status(hebrew_revival_requires_territorial_anchor, holdable).
narrative_ontology:cs_axiom_grounding('228d83a6-4c00-45aa-b0ff-034531947045', hebrew_revival_requires_territorial_anchor, empirically_contingent).
narrative_ontology:cs_reference_frame('228d83a6-4c00-45aa-b0ff-034531947045', jewish_cultural_diaspora_fragmentation).
narrative_ontology:cs_drift_state('228d83a6-4c00-45aa-b0ff-034531947045', early_mandate_period_post_1920, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('228d83a6-4c00-45aa-b0ff-034531947045', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_practitioners).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_speakers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_spiritual_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, arab_palestinian_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to establish and maintain Jewish cultural, spiritual, and educational institutions in Palestine—schools, theaters, publishing houses, religious academies, agricultural collectives rooted in Hebrew language and Jewish cultural traditions. Benefits from a territorial anchor where Jewish culture can develop without diaspora constraints. Not dependent on political sovereignty; cultural autonomy and institutional space suffice.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_practitioners, beneficiary,
    moderate, generational, mobile, regional).

% Bear the structural cost of Jewish settlement and institutional establishment in Palestine. Within the cultural zionism reading frame, this reading does not require Arab displacement or political subordination, but Arab communities experience land transfer, institutional marginalization, and demographic change as Jewish cultural infrastructure prioritizes Jewish participants. Exit options are constrained by economic ties to the land and political fragmentation.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, arab_palestinian_residents, payer,
    moderate, generational, constrained, regional).

% Set and enforce the legal framework within which Jewish cultural settlement occurs. Under Ottoman and then British Mandate administration, this role shifts institutional ground and enforcement capacity. The constraint's persistence depends on administrative permission and noninterference with cultural institution-building.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, ottoman_and_mandate_authorities, agenda_setter,
    institutional, biographical, analytical, regional).

% A subset of Jewish practitioners focused specifically on linguistic revival and cultural expression through Hebrew as a living language. Benefits from a territorial context where Hebrew becomes the ambient cultural medium. This group bridges cultural benefit and observational/analytical distance—they track whether language revival serves genuine coexistence or instrumentalizes it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_speakers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_speakers, observer).

% Would contest the framing that Jewish settlement and cultural institution-building can occur without political implications or Arab political voice. They are structurally excluded from the cultural zionism reading's joint-framework vision because that reading does not foreground Arab political sovereignty as a prerequisite, treating Arab objections as secondary to cultural autonomy. Their objections would reframe the constraint as political zionism dressed in cultural language.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, arab_intellectual_and_political_elites, excluded,
    powerful, generational, trapped, regional).

% Analytical observers who see the cultural zionism reading as compatible with (or even foundational to) a framework in which two national groups coexist institutionally without one requiring political majoritarianism. They track whether cultural autonomy can remain separable from sovereignty claims or whether the structural logic of settlement eventually folds culture into state-building.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, binational_framework_advocates, observer,
    moderate, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_practitioners).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__cultural_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes Jewish cultural, linguistic, educational, and spiritual institutions in Palestine as a territorial anchor for Jewish cultural renaissance and Hebrew revival, enabling Jewish cultural practices and community life to develop with institutional autonomy rather than diaspora constraints. The coordination problem solved: how to sustain Jewish cultural identity and creativity outside Europe in a territorial context without requiring political sovereignty or majority demographics as preconditions.
% TRANSFER_FUNCTION: Transfers land, institutional resources, and political-administrative latitude from Arab Palestinian communities and Ottoman/Mandate authorities to Jewish cultural practitioners and their institutions. Economic and social resources flow to Jewish cultural infrastructure; Arab communities experience land loss, administrative marginalization of Arab cultural institutions, and demographic pressure despite the reading's stated non-majoritarian intent.
% ABSENT_VOICES: Arab Palestinian intellectual and political elites who would argue that separating cultural settlement from political sovereignty is incoherent—that territorial control, land ownership, and institutional dominance inherently constitute political power. They are excluded because the cultural zionism reading does not foreground Arab political voice as a joint-framework requirement. Revisionist zionists would also object that cultural autonomy without sovereignty leaves the settlement vulnerable and incomplete.
% DISAPPEARANCE_RATIONALE: If the constraint and its institutional establishment vanished, Jewish cultural and linguistic institutions in Palestine would cease; Hebrew would not achieve territorial institutional density; Palestinian Arab communities would retain land and administrative control; the territorial anchor for Jewish cultural autonomy would be gone. The cultural renaissance that the reading instantiates depends on this constraint's persistence.
% FOUNDING_PROBLEM: Jewish culture and language face diaspora fragmentation and Europeanization; Hebrew language is liturgical and literary, not a living language of daily practice; Jewish cultural creativity is constrained by dependence on non-Jewish host societies and subject to periodic expulsion and persecution. A territorial anchor where Jewish cultural life can develop with institutional autonomy would allow Hebrew revival, cultural renaissance, and escape from diaspora vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Cultural zionism advocates attest the founding problem and the constraint's capacity to solve it. But rival readings (political zionism, labor zionism) and Arab observers attest that the founding problem, while real, is separable from territorial settlement in Palestine specifically—that Jewish cultural renaissance could occur elsewhere or that its occurrence in Palestine necessarily politicizes it. No corroboration from outside the zionist tradition attests that territorial settlement is the necessary solution to diaspora cultural fragmentation.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).
:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.22) because cultural zionism's core claim is non-majoritarian and non-extractive—cultural autonomy without political domination. It rises to 0.43 midway through the interval (around t=35) as the material reality of settlement accumulates: land acquisition accelerates, institutional infrastructure concentrates Jewish dominance, Arab communities experience marginalization despite the reading's stated coexistence intent. By interval end (t=50) it recalibrates slightly downward to 0.38, reflecting either stabilization of institutions into a genuinely shared cultural landscape, or measurement closer to the reading's stated frame after the initial rapid-change period. Suppression requirement remains low (0.22 at interval end) because the reading does not frame enforcement against Arab resistance as central; the constraint coordinates cultural benefit rather than coercive control. But suppression does rise modestly as Arab political objections intensify and the mandate authorities must actively permit Jewish settlement against Arab opposition. Theater ratio reflects the growing gap between the reading's cultural framing and the political implications of settlement: as cultural institutions proliferate, the performative element (claims that this is 'purely cultural' rather than political) becomes more elaborate. The reading's core claim is genuine—cultural renaissance is a real motivation—but the structural operation increasingly requires separation of cultural autonomy from Arab political voice, which is theatrical given the political reality. Measurements are authored on a single shared time grid so every metric is present at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the cultural practitioner and binational advocate seats, the constraint appears as genuine coordination: it solves the real problem of Jewish cultural diaspora fragmentation and enables coexistence without majoritarianism. From the Arab Palestinian seat, especially the political-elite seat, the same constraint operates as political settlement dressed in cultural language—land transfer, institutional dominance, and administrative control over space are inherently political regardless of cultural intent. The engine computes per-seat directionality from the structural data: Jewish cultural practitioners sit near the beneficiary end (low d); Arab Palestinians sit near the target end (high d); binational advocates sit near symmetric, recognizing both the genuine coordination benefit and the structural extraction. The mandate authorities hold moderate d depending on whether they prioritize administrative stability or Arab grievance.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish cultural practitioners are structural beneficiaries (they collect institutional space, cultural autonomy, territorial anchor; exit options are mobile—they can pursue cultural renaissance elsewhere but prefer Palestine). Arab Palestinians are targets despite the reading's non-majoritarian framing (they bear land loss and institutional marginalization; exit options are constrained by economic ties and political fragmentation). The mandate authorities hold moderate directionality—they extract administrative legitimacy from balancing both communities but face increasing pressure from Arab opposition. Binational advocates sit near symmetric because they both benefit from the model's intellectual coherence and bear the cost of witnessing its structural failure to separate culture from politics.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids false-positive mandatrophy by distinguishing genuine cultural coordination (language revival, institution-building, spiritual community) from pure extraction. The founding problem—Jewish cultural diaspora fragmentation—is real and substantively addressed by territorial anchoring. However, the constraint faces a specific mandatrophy risk: the founding problem is eventually divorced from the constraint's operation once cultural institutions are established. If Hebrew revives successfully and Jewish cultural institutions become stable, the ongoing land acquisition and Arab marginalization become more difficult to justify on cultural grounds—they persist for political/sovereignty reasons that the reading explicitly bracketed. This is a latent mandatrophy: the constraint began solving the founding problem but persists after that problem is partially solved, morphing into pure settlement accumulation. The measurement series capture this: extractiveness and theater rise together midway through the interval, signaling that the constraint's justificatory frame (cultural renaissance) and its actual operation (territorial control) diverge increasingly. Commentary tracks this but does not resolve it—that is the reading's own internal tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    culture_politics_separability,
    'Can Jewish cultural autonomy and institutional development genuinely be separated from political sovereignty and territorial control, or does the material reality of settlement inherently politicize cultural establishment?',
    'Historical counterfactual: if Jewish cultural institutions had flourished in Palestine under Arab or international political sovereignty (with guaranteed institutional autonomy but not Jewish political control), would the cultural renaissance have succeeded? Ethnographic and institutional analysis: are surviving cultural institutions sustained by cultural logic alone, or by underlying political authority?',
    'If separable, the cultural zionism reading is structurally coherent and extraction is modest (pure coordination cost). If inseparable, the reading is a framing device for political settlement, and extractiveness is higher (the real driving force is political, not cultural).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(culture_politics_separability, conceptual, 'Whether cultural and political dimensions of settlement are structurally separable or inherently entangled.').

omega_variable(
    binational_coexistence_feasibility,
    'Is a framework genuinely possible where Jewish cultural institutions flourish and Arab Palestinian communities maintain separate political and institutional sovereignty, or does the structural logic of settlement foreclose Arab political autonomy even when sovereignty is not explicitly claimed?',
    'Institutional analysis of Ottoman and Mandate period where both communities coexisted institutionally; counterfactual scenarios where cultural zionism was explicitly constrained to non-majoritarian institutional space; trajectory analysis of constraints that claimed cultural autonomy while eroding Arab institutional power.',
    'If feasible, the constraint models a genuinely non-extractive coordination possibility. If structurally foreclosed, the binational framing masks political majoritarianism, and the reading''s core axiom (non-majoritarian cultural autonomy) is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binational_coexistence_feasibility, empirical, 'Whether binational institutional coexistence can be sustained under cultural zionism without eroding Arab political voice.').

omega_variable(
    founding_problem_alternative_solutions,
    'Is territorial establishment in Palestine the necessary solution to Jewish cultural diaspora fragmentation, or could similar cultural renaissance have occurred through diaspora networks, linguistic preservation in situ, or cultural centers elsewhere?',
    'Comparative analysis of diaspora-based Jewish cultural flourishing (Enlightenment Hebrew literature, Hasidic renewal, American Jewish institutional development); counterfactual of non-territorial Hebrew revival. Testimony from cultural practitioners about whether territorial grounding is instrumentally necessary or ideologically preferred.',
    'If territorial establishment was necessary, the founding problem vindicates the constraint''s structure. If alternative paths existed, the constraint''s necessity is lessened, and its persistence reflects preference for settlement over cultural autonomy alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_alternative_solutions, conceptual, 'Whether territorial settlement is the only viable path to Jewish cultural renaissance or whether alternatives were foreclosed by choice rather than structural necessity.').

omega_variable(
    reading_vs_political_zionism_foreclosure,
    'Does the cultural zionism reading genuinely foreclose political zionism''s core premise (that Jewish statehood and sovereignty are necessary), or do the two readings ultimately converge once territorial institutions accumulate?',
    'Track whether cultural institutions eventually demanded political structures; examine whether early cultural zionists maintained non-sovereignty commitments or shifted toward state-building; analyze whether separating cultural from political was ever more than a transitional framing.',
    'If genuine foreclosure, cultural and political zionism are structurally distinct readings. If convergence is inevitable, cultural zionism is a temporary framing that political logic eventually overwhelms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_political_zionism_foreclosure, empirical, 'Whether cultural zionism remains structurally distinct from political zionism or whether institutional accumulation inevitably drives toward sovereignty claims.').

omega_variable(
    arab_marginalization_mechanism,
    'Is Arab institutional marginalization an incidental byproduct of Jewish cultural establishment, or is it a structural requirement of the constraint—does the coordination mechanism depend on Arab communities being subordinate to Jewish institutional dominance?',
    'Institutional analysis: were Arab schools, press, cultural organizations marginalized because resources were limited, or because Jewish institutional dominance was enforced? Comparative analysis: did similar magnitude of institutional development in other contexts produce comparable Arab marginalization? Counterfactual: could Jewish cultural institutions have flourished without constraining Arab institutional development?',
    'If incidental, suppression metrics are low and extraction is modest. If structural, suppression is higher and the constraint is more extractive than the reading acknowledges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_marginalization_mechanism, empirical, 'Whether Arab marginalization is incidental to Jewish cultural establishment or structurally required for the constraint''s operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jewi_tr_t8, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(jewi_tr_t16, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(jewi_tr_t25, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement(jewi_tr_t35, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement(jewi_tr_t50, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jewi_be_t8, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(jewi_be_t16, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(jewi_be_t25, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(jewi_be_t35, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 35, 0.43).
narrative_ontology:measurement(jewi_be_t50, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(jewi_su_t8, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 8, 0.15).
narrative_ontology:measurement(jewi_su_t16, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 16, 0.19).
narrative_ontology:measurement(jewi_su_t25, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 25, 0.23).
narrative_ontology:measurement(jewi_su_t35, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 35, 0.25).
narrative_ontology:measurement(jewi_su_t50, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 50, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__cultural_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel 'jewish_territorial_claim'. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and foundational axioms. Cultural zionism emphasizes cultural and linguistic renaissance without requiring sovereignty or Arab displacement (ε=0.38, claimed_type=rope). Political zionism emphasizes state sovereignty as necessary for Jewish safety (higher ε, claimed_type=tangled_rope, stronger enforcement). Labor zionism emphasizes socialist transformation and productive settlement (moderate ε, distinct beneficiary/victim structure). Revisionist zionism emphasizes maximalist territorial claims with military force (highest ε, claimed_type=snare). The four stories are linked via network.affects_constraints to enable constraint-family analysis: as one reading's material reality accumulates, it influences the pressure on sibling readings toward convergence or foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__cultural_zionism_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
