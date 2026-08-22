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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Ahad Ha'am Cultural-Spiritual Center Reading of Jewish Presence in Palestine
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates the cultural-Zionist reading of the contested
 *   kernel of Jewish sovereignty claims in Palestine — the tradition
 *   associated with Ahad Ha'am and later the Brit Shalom circle, which argued
 *   the Zionist project's core aim should be a Hebrew cultural and spiritual
 *   renaissance centered in Palestine, functioning as a wellspring for
 *   diaspora Jewish identity, without requiring political sovereignty over
 *   the land or a demographic majority displacing the existing Arab
 *   population. This is a distinct constraint from the liberal-nationalist,
 *   religious-Zionist, settler-colonial, and post-Zionist readings of the
 *   same underlying kernel — each of those is authored as its own story with
 *   its own epsilon, beneficiary structure, and type, per the
 *   epsilon-invariance principle. The cultural-Zionist reading's low authored
 *   extractiveness reflects its own internal logic: cultural presence,
 *   language revival, and institution-building do not intrinsically require
 *   displacing or subordinating co-inhabitants the way a demographic-majority
 *   or exclusive-sovereignty claim would. The theater_ratio's rise over the
 *   interval reflects the increasing gap between the cultural-center rhetoric
 *   still invoked by movement figures and the accelerating on-the-ground
 *   political-Zionist statehood project that this reading did not anticipate
 *   absorbing it.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Primary beneficiary (organized/mobile) — draws cultural renewal without relocating
 *   - yishuv_cultural_institutions: Agenda-setter (organized/constrained) — builds Hebrew cultural infrastructure
 *   - hebrew_language_revivalists: Beneficiary and co-agenda-setter (moderate/constrained) — does the linguistic labor
 *   - palestinian_arab_co_inhabitants: Excluded party (powerless/trapped) — envisioned as co-inhabitants but not consulted
 *   - political_zionist_factions: Observer/excluded from this reading's own framework (organized/constrained) — regards this reading as a stage to supersede
 *   - ottoman_then_british_administrators: Institutional observer (institutional/analytical) — permits or restricts institution-building
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
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Ahad Ha'am Cultural-Spiritual Center Reading of Jewish Presence in Palestine").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, 'e05c4b09-e624-497a-818f-62c6be8a29dd').
narrative_ontology:cs_kernel_codification('e05c4b09-e624-497a-818f-62c6be8a29dd', distributed).
narrative_ontology:cs_authority_grounding('e05c4b09-e624-497a-818f-62c6be8a29dd', distributed).
narrative_ontology:cs_reading_relation('e05c4b09-e624-497a-818f-62c6be8a29dd', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e05c4b09-e624-497a-818f-62c6be8a29dd', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('e05c4b09-e624-497a-818f-62c6be8a29dd', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e05c4b09-e624-497a-818f-62c6be8a29dd', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('e05c4b09-e624-497a-818f-62c6be8a29dd', foundational, cultural_vitality_sufficient_without_sovereignty).
narrative_ontology:cs_axiom_status(cultural_vitality_sufficient_without_sovereignty, overridden).
narrative_ontology:cs_axiom_grounding('e05c4b09-e624-497a-818f-62c6be8a29dd', cultural_vitality_sufficient_without_sovereignty, conventional).
narrative_ontology:cs_axiom('e05c4b09-e624-497a-818f-62c6be8a29dd', foundational, shared_cultural_space_compatible_with_co_inhabitation).
narrative_ontology:cs_axiom_status(shared_cultural_space_compatible_with_co_inhabitation, holdable).
narrative_ontology:cs_axiom_grounding('e05c4b09-e624-497a-818f-62c6be8a29dd', shared_cultural_space_compatible_with_co_inhabitation, empirically_contingent).
narrative_ontology:cs_reference_frame('e05c4b09-e624-497a-818f-62c6be8a29dd', ahad_haam_spiritual_center_thesis).
narrative_ontology:cs_drift_state('e05c4b09-e624-497a-818f-62c6be8a29dd', post_1948_statehood_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e05c4b09-e624-497a-818f-62c6be8a29dd', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_language_revivalists).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_peoplehood_as_cultural_nation).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_negation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scattered communities facing assimilation and cultural erosion look to a Hebrew-speaking, culturally revived center in Palestine as a spiritual and educational wellspring — a place to draw renewed language, literature, and collective memory from, without needing to emigrate or claim political rule over the land.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Schools, presses, theaters, and the Hebrew University build a modern Hebrew culture on the ground in Palestine. They set the cultural agenda — reviving language and letters — while explicitly declining to frame their project as demographic conquest or sovereign displacement of the existing population.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_cultural_institutions, agenda_setter,
    organized, generational, constrained, regional).

% Writers, teachers, and lexicographers labor to make Hebrew a living vernacular again. Their life's work depends on a functioning cultural center where the language is spoken daily; they benefit directly from institutional growth but bear the personal cost of pioneering a spoken language from liturgical remnants.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_language_revivalists, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_language_revivalists, agenda_setter).

% Long-resident population sharing the same land and, in this reading, envisioned as co-inhabitants of a shared cultural space rather than as a demographic obstacle to be outnumbered or displaced. Their own political voice on land tenure, governance, and national aspiration is not part of the cultural-center framework's deliberations, even though the cultural project unfolds on land they inhabit and often cultivate.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_co_inhabitants, excluded,
    powerless, generational, trapped, local).

% Factions pursuing statehood and demographic majority regard the cultural-center vision as insufficient or naive — a stage to be superseded rather than an end state. They watch the cultural project's institutions as infrastructure potentially repurposable toward sovereignty, without this reading itself endorsing that repurposing.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, political_zionist_factions, observer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, political_zionist_factions, excluded).

% Imperial administrators permit or restrict Jewish cultural and educational institution-building under evolving mandate and land policy, largely indifferent to the cultural-center framing itself but shaping what institutional growth is administratively possible.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, ottoman_then_british_administrators, observer,
    institutional, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__cultural_zionist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__cultural_zionist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Builds a shared cultural and spiritual reservoir — revived Hebrew language, literature, scholarship, religious and educational institutions — that diaspora Jewish communities everywhere can draw on to resist assimilation, without requiring political rule over the land or a demographic majority within it.
% TRANSFER_FUNCTION: Moves diaspora philanthropic resources, immigrant cultural labor, and intellectual attention toward institution-building in Palestine (schools, presses, the Hebrew University); in return, a renewed language and literary corpus flows back out to diaspora communities as a resource against cultural dissolution.
% ABSENT_VOICES: Palestinian Arab residents of the same land are structurally absent from this reading's own deliberations about what the cultural center should be and how it should relate to the people already living there; the reading imagines them as co-inhabitants but does not seat them as co-authors of the cultural project's terms.
% DISAPPEARANCE_RATIONALE: Cultural Zionists at the time argued that without a living center, Hebrew culture and diaspora Jewish identity would continue eroding toward assimilation — the world would rearrange around accelerated cultural loss. Critics both within and outside the movement argued the cultural project was already parasitic on, or a way-station toward, the political sovereignty project, such that its disappearance would barely register given how thoroughly political Zionism had already eclipsed it in practice by the 1930s-40s.
% FOUNDING_PROBLEM: Diaspora Jewish communities faced assimilation, pogrom-driven displacement, and the erosion of a shared cultural-linguistic core (Hebrew as living language, not just liturgy); Ahad Ha'am and cultural Zionists argued the crisis was spiritual and cultural before it was a crisis of statelessness, and needed a cultural-spiritual center of gravity rather than a sovereign state to solve it.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Zionist movement (e.g., scholarship on Ahad Ha'am and the Brit Shalom circle) attest the cultural-spiritual framing was a genuine, distinct current within the movement, not merely rhetoric later abandoned; contemporary post-Zionist and binationalist scholars, writing from outside the beneficiary set, corroborate that this reading existed as a minority position but was structurally marginalized once political Zionism's statehood project consolidated institutional and diplomatic power after 1917 and especially after 1948 — making the founding problem's 'still live' status genuinely contested rather than settled.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, contested).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.22 by 1948) because, by this reading's own structural logic, a cultural-spiritual center does not require land expropriation, demographic engineering, or political subordination of co-inhabitants — its coordination function (a shared cultural reservoir for diaspora communities) can in principle be realized alongside, not instead of, an existing population. Suppression is correspondingly low (0.18): the cultural project's own account does not depend on coercing exits or foreclosing Palestinian alternatives. The rising theater_ratio (0.12 to 0.30) captures a genuine internal tension within the historical movement: cultural-Zionist institutions persisted rhetorically even as political Zionism's sovereignty project increasingly set the movement's real trajectory after the Balfour Declaration (1917) and especially through the Mandate period — the cultural-center framing became progressively more performative relative to the political-sovereignty current it was nominally distinct from.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities and Hebrew revivalists are beneficiaries with low d — the constraint subsidizes their cultural and linguistic needs without extracting from them. Yishuv cultural institutions sit as agenda-setters, shaping the terms of the cultural project. Palestinian Arab co-inhabitants are declared excluded rather than victims in the base_properties structural data, consistent with this reading's own low-extraction self-conception — they are not named as bearing displacement costs WITHIN this reading's own terms, even though the excluded-voice structure documents that their absence from deliberation is itself a real structural fact the reading does not resolve. This is a deliberate feature of authoring this reading faithfully: the settler_colonial_reading (a sibling story) authors the same historical period with Palestinians as victims and much higher extractiveness — the two readings are not the same constraint measured differently, they are two different constraints per the epsilon-invariance principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The cultural-Zionist reading's founding problem (diaspora cultural erosion and assimilation) is genuinely contested as still-live: some corroborating sources hold it never fully materialized as intended since political Zionism's statehood project eclipsed it, others hold the Hebrew-language and cultural-institutional achievement (revived vernacular Hebrew, Hebrew University, modern Hebrew literature) represents the founding problem's actual and durable resolution, persisting as living infrastructure rather than as inertial performance. This is precisely why founding_problem_status is authored contested rather than dead or live — the mismatch-consumer logic (status vs. disappearance_verdict) should read this as a genuinely unresolved case rather than either a clean success or a captured zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_project_political_capture_ambiguity,
    'Was the cultural-spiritual center framing a genuinely independent, sustainable alternative to political sovereignty, or was it always structurally destined to be absorbed/superseded by the political-Zionist statehood project once demographic and diplomatic conditions allowed?',
    'Historical-institutional analysis of Brit Shalom and cultural-Zionist organizational funding, membership overlap with political-Zionist bodies (World Zionist Organization), and the trajectory of resource allocation between cultural institutions and political/paramilitary institutions from 1917-1948.',
    'If the cultural project was always subordinate to or a stepping-stone for the political project, this reading''s low extractiveness score describes a rhetorical layer rather than the operative constraint, and the settler_colonial_reading''s higher-extraction account of the same historical actors would better describe what was structurally operative beneath the cultural framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_project_political_capture_ambiguity, empirical, 'Whether cultural Zionism was an independent trajectory or a phase absorbed into political Zionism.').

omega_variable(
    co_inhabitant_consent_ambiguity,
    'Does this reading''s low extractiveness score presuppose Palestinian Arab consent or acquiescence to shared cultural space that was never actually sought or obtained, making the ''no displacement required'' premise normatively question-begging even on its own terms?',
    'Examination of contemporaneous cultural-Zionist writings (Ahad Ha''am''s own essays, Brit Shalom pamphlets) for explicit engagement with Arab political consent versus mere cultural coexistence assumptions; comparison with Palestinian Arab political writings and organizing of the same period documenting their own account of the arrangement.',
    'If the reading''s coexistence premise was never actually negotiated with or endorsed by Palestinian political representatives, the low suppression/extraction scores describe an aspirational self-conception rather than a jointly-agreed arrangement, which would push this reading''s classification closer to a rope claimed by only one party rather than a genuine mutual-benefit coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_inhabitant_consent_ambiguity, conceptual, 'Whether the shared-cultural-space premise required and received actual Palestinian consent.').

omega_variable(
    diaspora_negation_normative_status,
    'Is the vindicated proposition of diaspora cultural erosion (the ''negation of exile'' thesis underlying the founding problem) an empirically grounded description of assimilation dynamics, or a normatively loaded framing that pathologizes diaspora existence to justify the cultural-center project?',
    'Comparative demographic and sociological study of Jewish cultural continuity in diaspora communities that did not participate in the cultural-Zionist project, versus those that did, controlling for other historical factors (persecution, economic conditions).',
    'If diaspora cultural continuity was achievable without a Palestinian center, the founding problem''s premise is weaker than the reading assumes, which would weaken the coordination-function claim and shift the constraint''s descriptive profile toward lower genuine coordination value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_negation_normative_status, conceptual, 'Whether the diaspora-erosion premise justifying the cultural center is empirically sound or normatively loaded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1897, 0.12).
narrative_ontology:measurement(jewi_tr_t1907, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1907, 0.18).
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1917, 0.22).
narrative_ontology:measurement(jewi_tr_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1929, 0.26).
narrative_ontology:measurement(jewi_tr_t1939, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1939, 0.29).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1948, 0.3).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1897, 0.1).
narrative_ontology:measurement(jewi_be_t1907, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1907, 0.13).
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1917, 0.16).
narrative_ontology:measurement(jewi_be_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1929, 0.19).
narrative_ontology:measurement(jewi_be_t1939, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1939, 0.21).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1948, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_sovereignty_palestine__cultural_zionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__cultural_zionist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language kernel 'Jewish sovereignty in Palestine' per the epsilon-invariance principle. Each sibling reading (liberal_nationalist, settler_colonial, religious_zionist, post_zionist, and this cultural_zionist reading) authors a structurally distinct claim with its own epsilon, beneficiary/victim structure, and classification. This reading authors the lowest extractiveness of the family (0.22) because its own coordination logic explicitly rejects the zero-sum sovereignty/demographic-majority premise that drives extraction in the liberal_nationalist and religious_zionist readings, and that the settler_colonial reading identifies as displacement regardless of stated intent. The five stories are linked bidirectionally as a constraint family; none is a measurement of the others under a different observable — they are different constraints sharing a contested historical kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
