% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__native_generation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Native Generative Speech as the Criterion of Hebrew's Life (Ben-Yehuda / Yishuv Revival Reading)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested kernel 'Hebrew becomes a
 *   living language' — the native-generation reading, which holds that Hebrew
 *   is only genuinely alive once native speakers produce it generatively in
 *   daily speech, as their acquired mother tongue, rather than through
 *   memorized liturgical recitation or literary composition by non-native
 *   writers. This reading is structurally distinct from (not a measurement
 *   variant of) the liturgical-continuity reading, which locates Hebrew's
 *   life in unbroken recitation and textual study across the diaspora, and
 *   the literary-revival reading, which locates it in Haskalah written
 *   generative competence without native daily speech. Each reading names a
 *   different arrangement, with a different beneficiary/victim structure and
 *   a different epsilon; per the ε-invariance principle they are authored as
 *   three separate constraint files linked by network.affects_constraints,
 *   not as one story with a measurement parameter. This file's ε (0.58)
 *   reflects the native-generation reading's own arrangement: Hebrew-medium
 *   schooling, the Gdud Meginei HaSafah's social enforcement against Yiddish
 *   and Ladino in public life, and the domestic authority costs borne by
 *   immigrant parents — assessed by this reading's own lights, not relative
 *   to the liturgical or literary readings' endorsed alternatives.
 *
 * KEY AGENTS:
 *   - hebraist_revival_institutions: Sets and enforces the native-generative criterion via schooling and social policy (institutional/arbitrage) — administers the standard
 *   - sabra_native_speaker_generation: Primary beneficiary — native fluency confers full social and national standing (moderate/mobile)
 *   - yiddish_vernacular_speakers: Primary target — pressured to abandon their vernacular in public and private life (moderate/trapped)
 *   - ladino_vernacular_speakers: Secondary target, weaker organized voice, regional rather than national scope (powerless/trapped)
 *   - immigrant_parents_generation: Dual-positioned — benefits through children's integration, pays in domestic linguistic authority (moderate/constrained)
 *   - liturgical_hebrew_tradition_bearers: Excluded — their two-millennia recitation-based continuity is not engaged by the native-generation criterion (organized/constrained)
 *   - sociolinguistic_historians: Analytical observer of the revitalization case (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.58).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.71).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Native Generative Speech as the Criterion of Hebrew's Life (Ben-Yehuda / Yishuv Revival Reading)").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '602f51cf-e491-4906-8d1a-33ff38cd27ae').
narrative_ontology:cs_kernel_codification('602f51cf-e491-4906-8d1a-33ff38cd27ae', distributed).
narrative_ontology:cs_authority_grounding('602f51cf-e491-4906-8d1a-33ff38cd27ae', extraction).
narrative_ontology:cs_interpretation_layer_present('602f51cf-e491-4906-8d1a-33ff38cd27ae').
narrative_ontology:cs_reading_relation('602f51cf-e491-4906-8d1a-33ff38cd27ae', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('602f51cf-e491-4906-8d1a-33ff38cd27ae', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('602f51cf-e491-4906-8d1a-33ff38cd27ae', foundational, native_acquisition_is_the_only_valid_vitality_test).
narrative_ontology:cs_axiom_status(native_acquisition_is_the_only_valid_vitality_test, holdable).
narrative_ontology:cs_axiom_grounding('602f51cf-e491-4906-8d1a-33ff38cd27ae', native_acquisition_is_the_only_valid_vitality_test, empirically_contingent).
narrative_ontology:cs_axiom('602f51cf-e491-4906-8d1a-33ff38cd27ae', secondary, national_vernacular_unification_justifies_vernacular_suppression).
narrative_ontology:cs_axiom_status(national_vernacular_unification_justifies_vernacular_suppression, holdable).
narrative_ontology:cs_axiom_grounding('602f51cf-e491-4906-8d1a-33ff38cd27ae', national_vernacular_unification_justifies_vernacular_suppression, instrumental).
narrative_ontology:cs_reference_frame('602f51cf-e491-4906-8d1a-33ff38cd27ae', diaspora_multilingual_vernacular_equilibrium).
narrative_ontology:cs_drift_state('602f51cf-e491-4906-8d1a-33ff38cd27ae', yishuv_consolidation_1948, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('602f51cf-e491-4906-8d1a-33ff38cd27ae', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebraist_revival_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, sabra_native_speaker_generation).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, zionist_national_project).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, immigrant_parents_generation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, immigrant_parents_generation).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, language_revival_is_possible_through_native_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Hebrew Language Committee, Hebrew-language schools, and Yishuv civic bodies set curriculum and social policy so that children acquire Hebrew as a first, generative language rather than through recitation of prayers or texts. They administer the standard, credential Hebrew-medium teachers, and treat non-Hebrew home speech as a problem to be corrected.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebraist_revival_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Children raised in Hebrew-speaking households and Hebrew-medium schools acquire the language as their mother tongue, gaining full social, economic, and national standing within the Yishuv on the strength of that native fluency. Their fluency validates the reading and gives them the strongest claim to belonging.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, sabra_native_speaker_generation, beneficiary,
    moderate, biographical, mobile, national).

% The national project treats a living vernacular as proof of a living people entitled to a homeland; native generative Hebrew speech supplies exactly that proof. Not an actor itself, but the collective aim that the constraint's success is measured against.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_national_project, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__native_generation_reading, zionist_national_project).

% Adults who arrived fluent in Yiddish, the shared vernacular of Ashkenazi diaspora life, are pressured — through school policy, street-level Hebraist enforcement (the Gdud Meginei HaSafah), workplace expectation, and social stigma — to abandon Yiddish in public and at home. Yiddish is recast as exile-language, a mark of the old world to be shed rather than passed on.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_vernacular_speakers, payer,
    moderate, biographical, trapped, national).

% Sephardi immigrants whose home vernacular is Judeo-Spanish face the same reclassification: their inherited daily language does not count toward the revival's criterion of life, and their children are steered into Hebrew-medium schooling that treats Ladino transmission as an obstacle rather than an asset.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_vernacular_speakers, payer,
    powerless, biographical, trapped, regional).

% Parents who never acquired native generative Hebrew fluency themselves raise children who surpass and often correct them in the language of the household, inverting normal linguistic authority. They benefit from their children's integration into the new society but pay in domestic authority, intimacy of shared language, and cultural continuity with their own upbringing.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, immigrant_parents_generation, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, immigrant_parents_generation, beneficiary).

% Rabbinic and diaspora communities who maintained Hebrew through unbroken recitation and textual study for two millennia are not consulted on whether that continuity already constituted a living language; the native-generation criterion structurally disqualifies their claim without engaging it directly.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, liturgical_hebrew_tradition_bearers, excluded,
    organized, civilizational, constrained, global).

% Scholars of language revitalization examine the Hebrew case as the paradigmatic (and contested) instance of a language reclaiming native-speaker status, comparing it to failed and partial revitalization efforts elsewhere and assessing what the reachability break required.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, sociolinguistic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, hebraist_revival_institutions).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates an entire immigrant society around a single acquirable, generative daily vernacular so that people from dozens of countries of origin, with no shared spoken language, can raise children who communicate natively with one another and function as one linguistic community with a common national life.
% TRANSFER_FUNCTION: Moves linguistic capital, domestic authority, and social legitimacy from the diaspora vernaculars (chiefly Yiddish and Ladino) and their adult speakers to Hebrew and the children who acquire it natively; the transfer is enforced through schooling, street-level social pressure, and the prestige economy of the Yishuv.
% ABSENT_VOICES: Diaspora Yiddishist and Ladino cultural institutions, and the liturgical tradition-bearers who considered Hebrew already alive through recitation, were not seated in the bodies that set the native-generation criterion; their objection — that vernacular richness and textual continuity were being sacrificed to a single national-linguistic project — is documented mainly in retrospective cultural history, not in the founding deliberations.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion were dropped and Hebrew's revival were judged instead by literary output or liturgical continuity, the entire apparatus built around it — Hebrew-medium schooling, the language police, the stigmatization of Yiddish and Ladino, the credentialing of Hebrew fluency as the marker of belonging — would lose its organizing rationale; the social hierarchy it produced (native Hebrew speakers at the top, immigrant-vernacular speakers marked as transitional) would have no criterion left to enforce it.
% FOUNDING_PROBLEM: A dispersed people with dozens of mutually unintelligible home vernaculars needed a single common language that a modern national society could function in daily life, not merely study or recite — Ben-Yehuda and the Hebraist movement judged that only children raised as native speakers, producing Hebrew generatively rather than by rote, could complete that transition and make Hebrew answer to the demands of an actual modern vernacular.
% FOUNDING_PROBLEM_CORROBORATION: Israeli sociolinguists and historians of the Yishuv (outside the Hebraist institutions themselves) corroborate that the founding problem — lack of a common vernacular — was real and that native transmission did in fact occur within roughly two generations, an outcome independently documented in linguistic surveys of the Yishuv. The same outside historiography also corroborates that the criterion's enforcement actively suppressed Yiddish and Ladino transmission beyond what coordination alone required, a cost the revival institutions themselves rarely acknowledge as a cost rather than as necessary progress.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_living_language__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) tracks the transfer from diaspora-vernacular speakers to the Hebrew-native generation: it rises steadily from 1881 to 1948 as Hebrew-medium schooling and the language-enforcement apparatus matured, then plateaus as the transition completed and the enforcement had less remaining vernacular-speaker resistance to suppress. Suppression (0.71) is authored higher than extractiveness because the mechanism required active, organized social coercion — public shaming, the 'language guard' patrols, exclusion from Hebrew-medium institutions for those who persisted in Yiddish or Ladino — not merely economic pressure; this is a raw structural property, not scaled by scope. Theater ratio stays low (0.10 to 0.22) because the coordination function (a genuinely acquired, functioning national vernacular) was real and substantially achieved, not merely performed; this is not a piton. All three temporal metrics share the same six-point grid (1881, 1897, 1909, 1922, 1936, 1948) per the alignment rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebraist institutions sit at the beneficiary/agenda-setter pole: they set the criterion, administer its enforcement, and their success is validated by every native speaker produced. The sabra generation is a structural beneficiary — low d, the constraint (native transmission as the criterion of life) directly confers their status. Yiddish and Ladino vernacular speakers are structural targets — high d — because the same schooling and social-enforcement apparatus that produces native Hebrew speakers is what strips their own vernacular of transmission value and social legitimacy; their exit options are trapped (they cannot practically leave the Yishuv or the enforcement environment and still participate in its emerging national life). Immigrant parents are genuinely dual: they benefit from their children's belonging while paying in the currency of shared home language and domestic authority — hence the secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no common vernacular across a dispersed, multilingual immigrant population — was real and, by outside sociolinguistic corroboration, was substantively solved within roughly two generations; native Hebrew fluency did emerge and did stabilize into a functioning national vernacular. This is why the story is authored as tangled_rope rather than snare: the coordination function is genuine, not cover. But the same corroborating historiography documents that enforcement against Yiddish and Ladino continued and intensified beyond what pure coordination required, which is the tangled/extractive layer riding on the coordination core — status contested rather than flatly dead or live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_break_acknowledgment,
    'Does the native-generation reading require acknowledging a strict-reachability break — i.e., that no unbroken chain of native child-to-child transmission connects ancient spoken Hebrew to the Yishuv''s native speakers, making this a reconstruction rather than a continuation?',
    'Historical linguistic record: Hebrew had no continuous native-speaker community for roughly 1,700 years before the Yishuv revival; the reading''s own criterion (native generative production) must therefore treat the revival as a reconstruction event, not an unbroken continuity, which is a strictly different claim from the liturgical-continuity reading''s premise of unbroken life through recitation.',
    'If the reading is honest about the reconstruction, its coordination-function claim (rebuilding a vernacular from written/liturgical sources plus native-acquisition mechanisms) is a distinct and more remarkable achievement than the liturgical reading''s continuity claim, but it cannot also claim the liturgical reading''s unbroken-line legitimacy — the two readings are not compatible legitimating stories for the same fact pattern.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_break_acknowledgment, empirical, 'Whether this reading acknowledges Hebrew''s spoken-vernacular reachability break as reconstruction rather than continuity.').

omega_variable(
    coordination_vs_suppression_necessity,
    'Was suppressing Yiddish and Ladino transmission structurally necessary to achieve a unified native Hebrew vernacular, or was it excess enforcement beyond what coordination required?',
    'Comparative study of multilingual revitalization contexts where a target language achieved native-speaker status without suppressing competing home vernaculars (e.g., some Indigenous language immersion programs coexisting with heritage-language maintenance) versus the Yishuv''s more coercive approach.',
    'If suppression was excess (not structurally required for the coordination outcome), the tangled_rope classification is well-grounded — a real coordination core with an avoidable extractive overlay. If suppression was in fact necessary to prevent a bilingual/diglossic equilibrium from re-forming, the coordination and extraction functions are less separable than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_suppression_necessity, conceptual, 'Whether vernacular suppression was necessary to the coordination outcome or additive extraction.').

omega_variable(
    criterion_selection_as_constructed_choice,
    'Is ''native generative daily speech'' a natural, self-evident criterion for a language being ''alive,'' or a constructed choice that happens to favor the Zionist national project''s specific needs over the liturgical and literary communities'' criteria?',
    'Cross-linguistic comparison of how sociolinguists define language vitality (native acquisition is one of several standard criteria alongside intergenerational transmission, domain coverage, and vitality indices); assess whether the native-generation criterion was selected because it was analytically superior or because it was the only criterion the national project''s institutions could deliver and be credited for.',
    'If constructed to favor the institutions that could deliver it, the reading''s claim to define ''life'' for Hebrew (rather than merely describe one true thing about it) is itself part of the extractive structure — the criterion was chosen by the same body that then benefited from meeting it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(criterion_selection_as_constructed_choice, conceptual, 'Whether the native-generation criterion is self-evidently correct or an interested construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1881, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1881, hebrew_living_language__native_generation_reading, theater_ratio, 1881, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t1881, observed).
narrative_ontology:measurement(hebr_tr_t1897, hebrew_living_language__native_generation_reading, theater_ratio, 1897, 0.13).
narrative_ontology:measurement_basis(hebr_tr_t1897, observed).
narrative_ontology:measurement(hebr_tr_t1909, hebrew_living_language__native_generation_reading, theater_ratio, 1909, 0.16).
narrative_ontology:measurement_basis(hebr_tr_t1909, observed).
narrative_ontology:measurement(hebr_tr_t1922, hebrew_living_language__native_generation_reading, theater_ratio, 1922, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t1922, observed).
narrative_ontology:measurement(hebr_tr_t1936, hebrew_living_language__native_generation_reading, theater_ratio, 1936, 0.21).
narrative_ontology:measurement_basis(hebr_tr_t1936, observed).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_living_language__native_generation_reading, theater_ratio, 1948, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1881, hebrew_living_language__native_generation_reading, base_extractiveness, 1881, 0.28).
narrative_ontology:measurement_basis(hebr_be_t1881, observed).
narrative_ontology:measurement(hebr_be_t1897, hebrew_living_language__native_generation_reading, base_extractiveness, 1897, 0.38).
narrative_ontology:measurement_basis(hebr_be_t1897, observed).
narrative_ontology:measurement(hebr_be_t1909, hebrew_living_language__native_generation_reading, base_extractiveness, 1909, 0.47).
narrative_ontology:measurement_basis(hebr_be_t1909, observed).
narrative_ontology:measurement(hebr_be_t1922, hebrew_living_language__native_generation_reading, base_extractiveness, 1922, 0.53).
narrative_ontology:measurement_basis(hebr_be_t1922, observed).
narrative_ontology:measurement(hebr_be_t1936, hebrew_living_language__native_generation_reading, base_extractiveness, 1936, 0.56).
narrative_ontology:measurement_basis(hebr_be_t1936, observed).
narrative_ontology:measurement(hebr_be_t1948, hebrew_living_language__native_generation_reading, base_extractiveness, 1948, 0.58).
narrative_ontology:measurement_basis(hebr_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1881, hebrew_living_language__native_generation_reading, suppression_requirement, 1881, 0.35).
narrative_ontology:measurement_basis(hebr_su_t1881, observed).
narrative_ontology:measurement(hebr_su_t1897, hebrew_living_language__native_generation_reading, suppression_requirement, 1897, 0.5).
narrative_ontology:measurement_basis(hebr_su_t1897, observed).
narrative_ontology:measurement(hebr_su_t1909, hebrew_living_language__native_generation_reading, suppression_requirement, 1909, 0.62).
narrative_ontology:measurement_basis(hebr_su_t1909, observed).
narrative_ontology:measurement(hebr_su_t1922, hebrew_living_language__native_generation_reading, suppression_requirement, 1922, 0.68).
narrative_ontology:measurement_basis(hebr_su_t1922, observed).
narrative_ontology:measurement(hebr_su_t1936, hebrew_living_language__native_generation_reading, suppression_requirement, 1936, 0.7).
narrative_ontology:measurement_basis(hebr_su_t1936, observed).
narrative_ontology:measurement(hebr_su_t1948, hebrew_living_language__native_generation_reading, suppression_requirement, 1948, 0.71).
narrative_ontology:measurement_basis(hebr_su_t1948, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__native_generation_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language claim 'Hebrew becomes a living language,' per the ε-invariance principle. hebrew_living_language__liturgical_continuity_reading authors Hebrew's life as unbroken diaspora recitation/study (near-mountain: minimal extraction, no vernacular suppression, no reachability break). hebrew_living_language__literary_revival_reading authors Hebrew's life as Haskalah written generative competence (rope-leaning: coordination among writers, negligible vernacular suppression since it does not compete with home speech). This file, hebrew_living_language__native_generation_reading, authors the highest extraction of the three because its criterion is the only one requiring active suppression of competing home vernaculars to succeed. The three are linked here and should be linked reciprocally in both sibling files' network.affects_constraints arrays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
