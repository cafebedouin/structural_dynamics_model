% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Ahad Ha'am Cultural-Spiritual Center Vision (Cultural Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story authors the cultural Zionist reading associated with Ahad
 *   Ha'am's 'spiritual center' vision: a Hebrew cultural and spiritual
 *   renaissance in Palestine that renews diaspora Jewish identity without
 *   requiring political sovereignty over the territory or a demographic
 *   majority within it. The constraint is a genuine coordination structure on
 *   its own terms — diaspora communities, Hebrew writers, and settlers
 *   coordinating around language revival and institution-building — with
 *   correspondingly low extractiveness, since the reading's own premise
 *   disclaims the displacement mechanisms that would generate victims. This
 *   is deliberately NOT the liberal-nationalist, religious-Zionist,
 *   settler-colonial, or post-Zionist readings of the same underlying kernel
 *   (Jewish sovereignty/presence in Palestine); each of those is a
 *   structurally distinct constraint with its own ε and its own
 *   beneficiary/victim structure, linked here only through the shared
 *   kernel_id in cs_structure, never merged into this story's classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.22).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.18).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Ahad Ha'am Cultural-Spiritual Center Vision (Cultural Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, 'de8dee88-eb26-49f8-b20b-6affab62c0ee').
narrative_ontology:cs_kernel_codification('de8dee88-eb26-49f8-b20b-6affab62c0ee', distributed).
narrative_ontology:cs_authority_grounding('de8dee88-eb26-49f8-b20b-6affab62c0ee', distributed).
narrative_ontology:cs_reading_relation('de8dee88-eb26-49f8-b20b-6affab62c0ee', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('de8dee88-eb26-49f8-b20b-6affab62c0ee', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('de8dee88-eb26-49f8-b20b-6affab62c0ee', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('de8dee88-eb26-49f8-b20b-6affab62c0ee', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('de8dee88-eb26-49f8-b20b-6affab62c0ee', foundational, cultural_vitality_independent_of_sovereignty).
narrative_ontology:cs_axiom_status(cultural_vitality_independent_of_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('de8dee88-eb26-49f8-b20b-6affab62c0ee', cultural_vitality_independent_of_sovereignty, conventional).
narrative_ontology:cs_axiom('de8dee88-eb26-49f8-b20b-6affab62c0ee', foundational, spiritual_center_sufficient_without_demographic_majority).
narrative_ontology:cs_axiom_status(spiritual_center_sufficient_without_demographic_majority, overridden).
narrative_ontology:cs_axiom_grounding('de8dee88-eb26-49f8-b20b-6affab62c0ee', spiritual_center_sufficient_without_demographic_majority, empirically_contingent).
narrative_ontology:cs_reference_frame('de8dee88-eb26-49f8-b20b-6affab62c0ee', diaspora_cultural_continuity_crisis).
narrative_ontology:cs_drift_state('de8dee88-eb26-49f8-b20b-6affab62c0ee', post_1948_statehood, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('de8dee88-eb26-49f8-b20b-6affab62c0ee', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_settlers_in_yishuv).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_national_cultural_revival_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed across many countries, facing assimilation pressure and periodic persecution. Gains a living Hebrew cultural and spiritual reference point in Palestine — a center that renews language, literature, and religious practice without requiring emigration or political control. Can visit, study, fund institutions, or ignore the project entirely; participation is voluntary.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Schools, presses, and cultural societies building Hebrew as a living national language and secular-cultural identity in Palestine. Sets the agenda for what 'renaissance' means in practice — curricula, publishing, settlement of scholars and writers — without commanding an army or a state apparatus. Depends on continued immigration of committed cultural workers and diaspora funding.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_institutions, agenda_setter,
    moderate, generational, constrained, national).

% Jewish agricultural and urban settlers building communal and cultural institutions on land acquired through purchase. Benefits from a thickening Hebrew-speaking cultural milieu but, on this reading, is not pursuing demographic majority or sovereign control as the measure of success — success is a vital cultural minority presence, not displacement of the existing population.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_settlers_in_yishuv, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_settlers_in_yishuv, agenda_setter).

% Long-settled inhabitants of the land where the cultural center is being built. On this reading they are co-inhabitants of a shared cultural-geographic space rather than a population to be displaced or ruled over; the vision explicitly disclaims sovereignty and demographic majority as goals. In practice their consent to this arrangement was never solicited and their own national aspirations are not addressed by the cultural-center framing — they are structurally excluded from defining what the shared space means, even though the reading's own premise treats them as present rather than absent.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_residents, observer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_residents, excluded).

% Administer the territory under imperial mandate/rule and grant or withhold permits for land purchase, immigration, and institution-building. The cultural-center project depends on their continued tolerance but does not require them to cede governing authority, unlike sovereignty-seeking readings of the same kernel.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, ottoman_and_later_british_authorities, observer,
    institutional, biographical, analytical, regional).

% Contemporaneous currents within the broader movement arguing that only a sovereign state, not a cultural center, can secure Jewish safety and self-determination. They are excluded from this reading's own framing (which explicitly rejects sovereignty as the necessary goal) even though they compete for resources, immigrants, and legitimacy within the same movement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, political_zionist_factions, excluded,
    organized, generational, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diaspora Jewish communities, scholars, and settlers around building a shared linguistic and spiritual center — reviving Hebrew as a living cultural language and creating institutions (schools, presses, libraries) that give scattered Jewish communities a common cultural anchor, explicitly without requiring political sovereignty or a demographic majority in Palestine.
% TRANSFER_FUNCTION: Moves diaspora funding, immigrant cultural labor (writers, teachers, scholars), and attention toward institution-building in Palestine; in return moves a sense of cultural continuity, linguistic revival, and spiritual orientation back outward to dispersed Jewish communities. No territorial or governmental transfer is claimed by this reading.
% ABSENT_VOICES: Palestinian Arab residents are treated as co-inhabitants of the cultural-geographic space in the reading's own terms, but their own national, linguistic, and land claims are not addressed by the cultural-center framing and they had no voice in designing it. Political Zionist factions arguing for sovereignty are also excluded from this reading's self-definition, though they compete for the same diaspora resources.
% DISAPPEARANCE_RATIONALE: Cultural Zionists (Ahad Ha'am's tradition) would say the world rearranges substantially: without a living Hebrew cultural-spiritual center, diaspora Jewish identity loses a unifying secular-national anchor and assimilation pressure intensifies. Critics — including settler-colonial and post-Zionist readers of the same underlying kernel — would say the cultural project was always parasitic on or a way-station toward the political project, so its disappearance changes little that a sovereignty-focused reading would recognize as significant.
% FOUNDING_PROBLEM: Diaspora Jewish communities faced assimilation, cultural attenuation, and the erosion of Hebrew as a living language; Ahad Ha'am and cultural Zionists argued the solution was a spiritual-cultural renaissance centered in Palestine, not necessarily a sovereign state, because a state without cultural substance would be a body without a soul.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Zionism (e.g. scholarship on Ahad Ha'am and the Hebrew revival, largely outside the movement's own institutions) corroborate that a genuine cultural-linguistic revival occurred and was analytically distinct from the political-Zionist statehood program. However, the same historical record shows the cultural project was substantially absorbed into and instrumentalized by the eventual sovereignty project — so whether the 'founding problem' (cultural survival without political domination) remains live and independent, or was superseded and folded into statehood, is itself disputed by scholars outside both Zionist and anti-Zionist advocacy positions.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, contested).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.22 at 1948) because the cultural-center vision, taken on its own terms, does not require land dispossession, demographic engineering, or political subordination of the existing population to succeed — its success condition is a vital minority cultural presence, not majority control. It rises modestly over the interval as the Yishuv's institutions thicken and inevitably draw on land, labor, and imperial permitting that has real distributional consequences even absent an explicit sovereignty claim. Theater ratio rises slowly (0.15 to 0.30) reflecting the gap between the stated 'no sovereignty required' framing and the accumulating institutional apparatus (settlement, self-governing communal bodies, proto-state structures) that increasingly resembles state-building in function if not declared intent — this is the honest divergence the reading's own claim leaves unaddressed, not a verdict this story renders on it.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities and hebrew cultural institutions are declared beneficiaries: they gain a cultural-spiritual anchor and control the institution-building agenda, with voluntary, low-cost participation (mobile or constrained exit, no coercion required). Jewish settlers in the Yishuv are dual-positioned: beneficiaries of the cultural project and partial agenda-setters within it, though their exit options are more constrained by the biographical stakes of relocation. Palestinian Arab residents are NOT declared victims under this reading's own premises — the reading explicitly frames them as co-inhabitants, not a displaced population — but they are marked excluded/observer because the reading's design process never solicited their participation, which is a structural fact independent of whether this reading intends harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The cultural Zionist reading's founding problem (diaspora cultural attenuation, Hebrew language decline) could be judged largely resolved by 1948 — Hebrew was a living, standardized national language and diaspora institutions had a functioning cultural reference point — while the institutional apparatus built to solve it had already begun serving purposes (proto-state governance, land consolidation) beyond the original cultural-spiritual mandate. This is exactly the mandatrophy pattern the founding_problem_status/disappearance_verdict mismatch is designed to surface: status is contested rather than cleanly 'dead' precisely because the apparatus's function shifted without an explicit sunset or handoff.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_political_zionism_separability,
    'Was the cultural-spiritual center project genuinely separable from the political-sovereignty project, or was it always a stepping-stone/legitimating cover for eventual statehood?',
    'Close historical analysis of Ahad Ha''am''s own writings and institutional decisions versus the actual trajectory of Yishuv institutions 1897-1948 — did cultural institutions resist absorption into state-building apparatus, or merge into it without friction?',
    'If genuinely separable, this reading is a distinct, low-extraction coordination structure. If it was functionally a way-station to sovereignty, the low ε authored here understates the reading''s actual historical role and it should be read as continuous with the liberal_nationalist_reading rather than as an independent constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_political_zionism_separability, conceptual, 'Whether cultural Zionism was analytically and practically independent of the sovereignty project.').

omega_variable(
    coinhabitant_framing_versus_practice,
    'Did the ''co-inhabitants in shared cultural space'' framing correspond to actual practice regarding Palestinian Arab land, labor markets, and political voice, or did it function as a description that outpaced the institutions'' actual treatment of the existing population?',
    'Comparative study of land purchase and labor practices (e.g., the ''Hebrew labor'' movement) against the stated non-displacement, non-majoritarian premise of cultural Zionism specifically (as distinct from political Zionism).',
    'If practice diverged sharply from the stated framing, the low extractiveness authored here — grounded in the reading''s own premises — would need reassessment even while remaining internally consistent with the reading''s self-understanding; this is the seam where a cultural_zionist claim and a settler_colonial reading''s metrics would sharply diverge despite describing overlapping historical events.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coinhabitant_framing_versus_practice, empirical, 'Gap between the co-inhabitant framing and observed settlement/labor practice.').

omega_variable(
    beneficiary_naturalization_risk,
    'Does treating ''jewish_national_cultural_revival_doctrine'' as a vindicated proposition rather than examining whether the cultural project itself became a beneficiary-serving narrative risk naturalizing a contested historical claim?',
    'Cross-reference with post_zionist_reading''s assessment of whether the founding cultural-national narrative now functions primarily to legitimate the present state rather than to describe an ongoing cultural process.',
    'If the doctrine functions mainly as retrospective legitimation, this reading''s clean coordination framing may be doing more legitimating work in the present than its low ε suggests for the 1897-1948 interval alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_naturalization_risk, conceptual, 'Risk that the vindicated proposition doubles as present-day legitimation rather than pure historical description.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1897, 0.15).
narrative_ontology:measurement(jewi_tr_t1907, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1907, 0.18).
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1929, 0.24).
narrative_ontology:measurement(jewi_tr_t1939, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1939, 0.27).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1948, 0.3).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1897, 0.1).
narrative_ontology:measurement(jewi_be_t1907, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1907, 0.13).
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1917, 0.17).
narrative_ontology:measurement(jewi_be_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1929, 0.19).
narrative_ontology:measurement(jewi_be_t1939, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1939, 0.2).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1948, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_sovereignty_palestine__cultural_zionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the jewish_sovereignty_palestine kernel, each authored as a structurally distinct constraint per the ε-invariance principle. The cultural_zionist_reading is authored with markedly lower ε than the settler_colonial_reading because the two readings differ on the central empirical/normative question of whether displacement is constitutive of the arrangement — they are not the same constraint measured two ways, but two constraints sharing a kernel and a historical period. The liberal_nationalist_reading is the closest structural neighbor (both frame the project as legitimate collective self-determination) but differs on whether sovereignty is necessary to the project's success condition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
