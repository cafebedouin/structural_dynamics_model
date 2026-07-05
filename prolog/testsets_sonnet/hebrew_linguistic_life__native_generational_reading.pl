% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__native_generational_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Native-Acquisition Standard of Linguistic Vitality (Hebrew Revival Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'Hebrew linguistic
 *   life' kernel: that a language is alive only when children acquire it as a
 *   native mother tongue and use it across all daily secular functions. Under
 *   this reading, Hebrew was genuinely dead as a spoken vernacular from
 *   roughly 70 CE to 1880 CE — the dormancy period is treated as real, not
 *   merely liturgically continuous — and its 20th-century revival required
 *   active, coercive displacement of the actual living mother tongues of the
 *   immigrant generations (principally Yiddish, Ladino, and Jewish Arabic
 *   dialects). The victim set under this reading is linguistic diversity
 *   itself: speakers who were coerced, through school policy, social stigma,
 *   and organized enforcement (e.g. the Gdud Meginei HaSafa 'Language Defense
 *   Battalion'), into abandoning transmission of their native languages to
 *   their children. This is a distinct constraint from the
 *   liturgical-preservation reading (which would deny the dormancy ever
 *   occurred, since sacred use never ceased) and from the marketplace-pidgin
 *   reading (which would count Hebrew's earlier limited use as an
 *   inter-communal trade/prayer medium as already 'alive'). Each reading has
 *   a different epsilon: this one is substantially extractive because it
 *   names a victim set and an enforcement apparatus; the liturgical reading
 *   would likely register as near-mountain (continuous transmission, minimal
 *   coercion); the pidgin reading would likely register as a thin rope
 *   (low-stakes functional coordination, few victims).
 *
 * KEY AGENTS:
 *   - zionist_nation_building_institutions: agenda_setter (institutional/arbitrage) — administers the native-acquisition standard and collects the nation-building payoff
 *   - hebrew_revival_pedagogues: beneficiary/agenda_setter (organized/mobile) — professional and ideological authority built on the standard's success
 *   - sabra_generation_speakers: beneficiary (moderate/mobile) — inherit the functioning native vernacular
 *   - yiddish_speaking_immigrants: payer (powerless/trapped) — native transmission actively suppressed
 *   - ladino_speaking_immigrants: payer (powerless/trapped) — native transmission actively suppressed
 *   - arabic_speaking_jewish_immigrants: payer (powerless/trapped) — native transmission actively suppressed
 *   - diaspora_liturgical_communities: excluded (organized/constrained) — their unbroken liturgical transmission is read as insufficient under this standard
 *   - sociolinguistic_historians: observer (analytical/analytical) — documents the revival and its costs from outside the nation-building project
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.58).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.71).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Native-Acquisition Standard of Linguistic Vitality (Hebrew Revival Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, 'cdd6e1cd-507a-406d-a628-cac90dd4ba66').
narrative_ontology:cs_kernel_codification('cdd6e1cd-507a-406d-a628-cac90dd4ba66', distributed).
narrative_ontology:cs_authority_grounding('cdd6e1cd-507a-406d-a628-cac90dd4ba66', distributed).
narrative_ontology:cs_reading_relation('cdd6e1cd-507a-406d-a628-cac90dd4ba66', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdd6e1cd-507a-406d-a628-cac90dd4ba66', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('cdd6e1cd-507a-406d-a628-cac90dd4ba66', foundational, native_child_acquisition_is_necessary_for_life).
narrative_ontology:cs_axiom_status(native_child_acquisition_is_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('cdd6e1cd-507a-406d-a628-cac90dd4ba66', native_child_acquisition_is_necessary_for_life, conventional).
narrative_ontology:cs_axiom('cdd6e1cd-507a-406d-a628-cac90dd4ba66', secondary, secular_mundane_use_is_required_not_merely_sacred_use).
narrative_ontology:cs_axiom_status(secular_mundane_use_is_required_not_merely_sacred_use, holdable).
narrative_ontology:cs_axiom_grounding('cdd6e1cd-507a-406d-a628-cac90dd4ba66', secular_mundane_use_is_required_not_merely_sacred_use, conventional).
narrative_ontology:cs_reference_frame('cdd6e1cd-507a-406d-a628-cac90dd4ba66', pre_revival_multilingual_diaspora_vernaculars).
narrative_ontology:cs_drift_state('cdd6e1cd-507a-406d-a628-cac90dd4ba66', post_1948_statehood, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cdd6e1cd-507a-406d-a628-cac90dd4ba66', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_nation_building_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revival_pedagogues).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, sabra_generation_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jewish_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, diaspora_liturgical_communities).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, language_revival_is_possible).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, vernacular_transmission_is_necessary_condition_for_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Yishuv's educational, military, and civil administration bodies (Va'ad HaLashon, later the Academy of the Hebrew Language, the school system, the army) set curricula and social norms mandating Hebrew as the sole medium of instruction and daily public life, actively discouraging or banning other Jewish languages in schools, youth movements, and official settings. They administer the standard that counts a language as alive only if children acquire it natively and use it for mundane speech, and they collect the nation-building payoff: a unified national vernacular replacing linguistic fragmentation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_nation_building_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Teachers, lexicographers, and ideologues (in the Eliezer Ben-Yehuda tradition) who built the pedagogical and lexical infrastructure for native acquisition gain professional standing, canonical authority, and institutional posts from establishing the native-generational standard as correct. Their careers and legacy are constituted by the revival's success being measured this way.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revival_pedagogues, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, hebrew_revival_pedagogues, agenda_setter).

% The first generation of children raised with Hebrew as mother tongue inherit a functioning native vernacular, full social and economic participation in the new national community, and freedom from the stigma attached to diaspora languages. They did not choose the standard but are its structural beneficiaries.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sabra_generation_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Ashkenazi immigrants for whom Yiddish was the actual mother tongue and vehicle of daily secular and religious life for centuries. Under the native-generational standard, their children were pushed toward Hebrew monolingualism in schools and youth movements; Yiddish was actively marginalized as a language of the ghetto, sometimes met with organized public shaming (the 'language wars' and Gdud Meginei HaSafa patrols). They had no real institutional venue to contest the standard once it became the criterion for national belonging.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_immigrants, payer,
    powerless, biographical, trapped, national).

% Sephardi immigrants whose Judeo-Spanish vernacular was likewise excluded from the new definition of linguistic vitality; their transmission chains to children were broken within roughly one generation as Hebrew became the sole legitimate mother tongue in schools and public institutions, with no comparable revival infrastructure built for Ladino.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speaking_immigrants, payer,
    powerless, biographical, trapped, national).

% Jewish immigrants from Arab lands whose native Arabic dialects were suppressed and stigmatized in absorption centers and schools under the same native-generational Hebrew standard, compounding cultural erasure with the standard's insistence that only Hebrew acquisition by children counted as legitimate linguistic continuity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jewish_immigrants, payer,
    powerless, biographical, trapped, national).

% Communities maintaining Hebrew as a liturgical and scholarly language across the dormancy centuries — without native child acquisition — are read by this standard as having kept a 'dead' language alive only in form, not in fact. They are not consulted in setting the native-acquisition criterion and would object that their unbroken transmission chain constitutes a different, equally valid form of life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, diaspora_liturgical_communities, excluded,
    organized, civilizational, constrained, global).

% Scholars documenting the revival as a unique case of large-scale planned language revitalization; they assess the criterion's internal coherence and its human costs without being party to the nation-building project that gains from applying it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sociolinguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, zionist_nation_building_institutions).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a single native vernacular for a rapidly assembling, linguistically fragmented immigrant population solves a genuine coordination problem: without a shared mother tongue transmitted to children, a national society drawn from dozens of linguistic backgrounds cannot function as one polity across generations.
% TRANSFER_FUNCTION: The standard transfers linguistic capital, intergenerational transmission legitimacy, and social status from diaspora vernaculars (Yiddish, Ladino, Judeo-Arabic dialects) and their speakers to Hebrew and its institutional architects; children's mother-tongue formation is redirected en masse from parental languages to the state-sponsored language.
% ABSENT_VOICES: The immigrant generations whose own mother tongues were displaced had limited institutional voice in setting the criterion that judged their languages 'not alive' in the relevant sense; their descendants, now largely Hebrew monolingual, inherit the standard's benefits without direct memory of what a contrary criterion would have preserved.
% DISAPPEARANCE_RATIONALE: If the native-generational criterion were abandoned as the operative definition of linguistic life, the revival's central achievement claim (Hebrew as a 'living language' comparable to any national vernacular) would lose its distinguishing force relative to the liturgical-preservation reading, and the historical suppression of Yiddish/Ladino/Judeo-Arabic transmission would be visible as a cost rather than a necessary byproduct of revival — school curricula, national mythology, and linguistic policy debates would all require re-justification.
% FOUNDING_PROBLEM: A dispersed, multilingual Jewish immigrant population needed a shared, modern, secular vernacular to function as a cohesive national society, and existing diaspora languages were seen (by revivalists) as either sacred-only, foreign-marked, or insufficiently modern for that role.
% FOUNDING_PROBLEM_CORROBORATION: Israeli sociolinguists and historians of the Yiddish and Ladino press (writing from outside the Hebraist institutions that benefited from the standard) corroborate that the coordination problem was real but document that the specific criterion adopted — native child acquisition as the sole test of vitality — was also the instrument used to actively suppress rival transmission chains rather than merely coexist with them; Hebraist-aligned historiography treats the problem as fully and unambiguously resolved by the revival's success, which is the contested half.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__native_generational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__native_generational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from near-zero in 1880 (before the standard had institutional teeth) to 0.58 by 1948 as the Yishuv's educational and civil apparatus matured and the criterion became the operative test of belonging, then plateaus post-statehood as the transition completes and the extraction becomes structural rather than actively expanding. Suppression rises faster and higher than extraction (peaking near 0.72) because enforcement — school-language mandates, public shaming campaigns, youth-movement policy — was the primary mechanism, not incidental to it; this reflects the tangled_rope requirement that active enforcement is structurally necessary, not optional. Theater ratio stays comparatively low and rises only slowly (0.05 to 0.22) because the coordination function (a working national vernacular) is genuinely functional throughout, not merely performed — this is not a piton. Accessibility collapse (0.62) reflects that once Hebrew monolingual schooling became normalized, alternatives for transmitting Yiddish/Ladino/Arabic natively to children became very difficult to access, though not fully impossible (some communities retained partial transmission). Resistance (0.55) reflects real organized pushback — Yiddishist cultural movements, Sephardi community objections — that was present but ultimately overridden by state-building momentum.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and revival pedagogues sit at the low-d beneficiary end: they set the criterion, administer its enforcement, and collect its legitimating payoff (a nation with a 'living' vernacular). Sabra-generation speakers are beneficiaries by inheritance rather than choice, but structurally still gain full participation without bearing the transmission loss themselves. Yiddish, Ladino, and Judeo-Arabic speaking immigrants sit at the high-d target end: trapped exit options (their children's linguistic formation could not realistically be exited from the national school system), powerless power atom, and directly named as bearing the cost of the standard's enforcement. Diaspora liturgical communities are excluded rather than extracted from in the direct financial sense — their cost is definitional erasure (their form of continuity is not recognized as 'life' under this reading) rather than material transfer, which is why they carry the excluded role rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating a linguistically fragmented immigrant population around a shared modern vernacular — was genuinely live in 1880-1948 and is honestly resolved by 1960 (Hebrew functions as a native, mundane vernacular for a national population). This is why the classification is tangled_rope rather than snare: there is a real, non-fabricated coordination function, not merely an extraction story dressed as coordination. The mandatrophy risk runs the other direction from the usual case — not that a dead function persists as inertia, but that the standard's now-resolved success is retroactively used to fully justify the historical suppression as costless, when the victim set (permanently broken transmission chains for Yiddish, Ladino, and Judeo-Arabic to subsequent generations) is a real and non-reversible cost that persists as a structural fact independent of whether the coordination problem itself is now solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_period_is_kernel_contested,
    'Was Hebrew genuinely ''dead'' from 70-1880 CE under any coherent standard, or does the appearance of dormancy depend entirely on adopting the native-generational criterion this story authors?',
    'This is precisely the kernel contest: the liturgical_preservation_reading (sibling constraint) denies the dormancy occurred by using a different criterion (unbroken sacred transmission). No empirical fact resolves which criterion is correct — it is a definitional choice about what ''linguistic life'' means, routed here as a conceptual omega rather than adjudicated within this story.',
    'If the native-generational criterion is accepted as the only valid standard, the revival narrative and its associated victim set (this story) is the correct frame. If the liturgical criterion is accepted instead, there was no death and no revival to have costs — a structurally different constraint (the sibling file) applies instead, with negligible extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_period_is_kernel_contested, conceptual, 'Whether the dormancy period is a fact about Hebrew or an artifact of which kernel reading is adopted.').

omega_variable(
    sibling_reading_delta_pidgin,
    'How would extraction and victim structure differ under the marketplace_pidgin_reading, which counts inter-communal functional use (without native acquisition) as sufficient for life?',
    'Author the sibling constraint (marketplace_pidgin_reading) as its own file with its own epsilon and stakeholder set; compare victim sets directly rather than adjusting this file.',
    'The pidgin reading would likely find Hebrew ''alive'' throughout much of the dormancy period (as a limited trade/liturgical/inter-communal medium among diaspora Jewish communities), collapsing this story''s entire revival-and-suppression narrative and yielding a much smaller or absent victim set — a structurally distinct, low-extraction constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_pidgin, conceptual, 'How the marketplace-pidgin sibling reading would restructure the victim set and epsilon.').

omega_variable(
    counterfactual_coordination_without_suppression,
    'Could the coordination function (a shared modern national vernacular) have been achieved via a bilingual or multilingual model, without displacing Yiddish/Ladino/Arabic native transmission, and was that path foreclosed by institutional choice rather than necessity?',
    'Comparative study of contemporaneous multilingual nation-building efforts (e.g. early Indian, Belgian, or Swiss language policy) that achieved administrative coordination without mandating monolingual native acquisition; assess whether Yishuv institutions considered and rejected such models.',
    'If a lower-suppression path was structurally available and rejected, the tangled_rope''s enforcement intensity is better read as excess extraction beyond what coordination required, strengthening the victim-cost side of the classification. If no such path was administratively viable given the specific fragmentation and refugee conditions, the enforcement is closer to necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_coordination_without_suppression, empirical, 'Whether the suppression of rival mother tongues was necessary to the coordination function or excess to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1895, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1895, 0.08).
narrative_ontology:measurement(hebr_tr_t1910, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1910, 0.12).
narrative_ontology:measurement(hebr_tr_t1925, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1925, 0.15).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1940, 0.18).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1960, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(hebr_be_t1895, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1895, 0.28).
narrative_ontology:measurement(hebr_be_t1910, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1910, 0.42).
narrative_ontology:measurement(hebr_be_t1925, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1925, 0.51).
narrative_ontology:measurement(hebr_be_t1940, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1940, 0.56).
narrative_ontology:measurement(hebr_be_t1948, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1948, 0.58).
narrative_ontology:measurement(hebr_be_t1960, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1960, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.1).
narrative_ontology:measurement(hebr_su_t1895, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1895, 0.35).
narrative_ontology:measurement(hebr_su_t1910, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1910, 0.55).
narrative_ontology:measurement(hebr_su_t1925, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1925, 0.68).
narrative_ontology:measurement(hebr_su_t1940, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1940, 0.72).
narrative_ontology:measurement(hebr_su_t1948, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1948, 0.71).
narrative_ontology:measurement(hebr_su_t1960, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1960, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This file is one of three readings of the hebrew_linguistic_life kernel. native_generational_reading (this file) treats the 70-1880 CE period as genuine dormancy and the revival as a coercive intervention with a real victim set (Yiddish/Ladino/Judeo-Arabic speakers); liturgical_preservation_reading denies the dormancy occurred at all by using unbroken sacred transmission as the life criterion, yielding near-mountain epsilon; marketplace_pidgin_reading locates life in inter-communal functional use independent of native acquisition, yielding thin-rope epsilon with a much smaller victim set. The three are linked here rather than merged because each adopts a genuinely different observable for 'linguistic life,' producing different epsilon values per the epsilon-invariance principle — this is not one constraint measured three ways but three constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
