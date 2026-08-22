% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Native-Acquisition Standard for Hebrew Linguistic Vitality (Ben-Yehuda Revival Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the native-generational reading of the contested
 *   'is Hebrew alive' kernel: a language is alive only when children acquire
 *   it as a mother tongue and use it across all daily, secular functions.
 *   Under this reading, Hebrew was genuinely dead from roughly 70 CE (loss of
 *   vernacular daily use following the destruction of the Second Temple and
 *   dispersal) until Eliezer Ben-Yehuda's revival project beginning around
 *   1880, because for eighteen centuries no children anywhere acquired Hebrew
 *   as a first language for full daily use — it survived only as a
 *   liturgical, literary, and inter-communal language. The revival required
 *   deliberate, coercive language planning: Hebrew-only schools, youth
 *   movements, and social sanction against home use of Yiddish, Ladino, and
 *   Judeo-Arabic among immigrant families in the Yishuv. This reading's ε is
 *   authored for the standing arrangement under contest — the
 *   native-acquisition criterion as administered by Zionist national
 *   institutions from 1880 to 1948 — assessed by this reading's own lights,
 *   not for an idealized multilingual alternative.
 *
 * KEY AGENTS:
 *   - hebrew_language_council: institutional agenda-setter, codifies and enforces the native-acquisition standard
 *   - sabra_generation_children: primary beneficiaries, acquire functioning Hebrew mother tongue
 *   - zionist_national_movement: organized beneficiary, uses linguistic revival as evidence of national restoration
 *   - yiddish/ladino/arabic-speaking immigrants: powerless payers, lose intergenerational transmission of home languages
 *   - linguists_and_sociolinguists: analytical observers studying the revival as a case study
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.62).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.71).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Native-Acquisition Standard for Hebrew Linguistic Vitality (Ben-Yehuda Revival Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, 'dee9938d-2edf-4b4a-8256-fac8fd861e21').
narrative_ontology:cs_kernel_codification('dee9938d-2edf-4b4a-8256-fac8fd861e21', distributed).
narrative_ontology:cs_authority_grounding('dee9938d-2edf-4b4a-8256-fac8fd861e21', distributed).
narrative_ontology:cs_reading_relation('dee9938d-2edf-4b4a-8256-fac8fd861e21', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('dee9938d-2edf-4b4a-8256-fac8fd861e21', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('dee9938d-2edf-4b4a-8256-fac8fd861e21', foundational, native_acquisition_is_necessary_for_life).
narrative_ontology:cs_axiom_status(native_acquisition_is_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('dee9938d-2edf-4b4a-8256-fac8fd861e21', native_acquisition_is_necessary_for_life, conventional).
narrative_ontology:cs_axiom('dee9938d-2edf-4b4a-8256-fac8fd861e21', foundational, dormancy_without_native_speakers_constitutes_death).
narrative_ontology:cs_axiom_status(dormancy_without_native_speakers_constitutes_death, holdable).
narrative_ontology:cs_axiom_grounding('dee9938d-2edf-4b4a-8256-fac8fd861e21', dormancy_without_native_speakers_constitutes_death, empirically_contingent).
narrative_ontology:cs_reference_frame('dee9938d-2edf-4b4a-8256-fac8fd861e21', pre_diaspora_vernacular_hebrew).
narrative_ontology:cs_drift_state('dee9938d-2edf-4b4a-8256-fac8fd861e21', post_ben_yehuda_revival, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('dee9938d-2edf-4b4a-8256-fac8fd861e21', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_national_movement).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_language_council).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, sabra_generation_children).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jewish_immigrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies vocabulary, grammar, and pronunciation standards and campaigns for exclusive Hebrew use in homes, schools, and street life in Ottoman and Mandate Palestine. Administers the criterion that only child-native-acquisition plus full daily-domain coverage counts as linguistic life, and treats every other vernacular in the Yishuv as an obstacle to be displaced.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_language_council, agenda_setter,
    institutional, generational, analytical, national).

% Grow up acquiring Hebrew as a first language through Hebrew-only kindergartens, youth movements, and street play organized specifically to manufacture native speakers where none existed a generation before. They receive a functioning mother tongue and a national identity built around it; they did not choose the linguistic environment engineered around their childhoods.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sabra_generation_children, beneficiary,
    moderate, biographical, constrained, national).

% Uses the native-acquisition standard to demonstrate that Jewish national revival is not merely political but organically rooted in restored peoplehood, converting a linguistic-planning project into evidence for nation-building claims. Sets social and institutional incentives (employment, schooling, land allocation networks) that reward Hebrew monolingualism in the home.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_national_movement, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, zionist_national_movement, agenda_setter).

% Arrive as adults with Yiddish as their only fluent language, raising children in an environment where Yiddish is publicly shamed, excluded from Hebrew-medium schools, and treated as diaspora residue to be shed. Cannot pass Yiddish to their children as a first language without social and institutional penalty; watch their mother tongue fail the native-acquisition test by design, in their own household, within one generation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_immigrants, payer,
    powerless, biographical, trapped, national).

% Sephardi families whose home language, Ladino, is likewise excluded from the school system and civic life under the same native-acquisition standard applied uniformly across all incoming Jewish vernaculars. Their children are absorbed into Hebrew-medium instruction and the community's transmission chain is broken within a generation, despite Ladino's own centuries of continuous daily vernacular use.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speaking_immigrants, payer,
    powerless, biographical, trapped, national).

% Mizrahi and Yemenite Jewish immigrants whose Judeo-Arabic vernaculars are treated as doubly suspect — associated with the surrounding non-Jewish population as well as failing the native-Hebrew standard — and are actively discouraged in schools and public institutions, accelerating language shift within one to two generations.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jewish_immigrants, payer,
    powerless, biographical, trapped, national).

% Study the Hebrew revival as the paradigm case for whether language death is reversible, applying the native-acquisition criterion analytically rather than politically, while documenting the same episode's cost to the diaspora vernaculars it displaced.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguists_and_sociolinguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, zionist_national_movement).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, shared, functioning national vernacular for a population arriving with dozens of mutually unintelligible mother tongues, solving a genuine coordination problem: without a common daily language, a unified civic, educational, and economic life among the incoming Jewish population would have been very difficult to build.
% TRANSFER_FUNCTION: Moves linguistic capital and intergenerational transmission rights away from immigrant home languages (Yiddish, Ladino, Judeo-Arabic) and concentrates them into a single revived language administered by Zionist national institutions; the children of immigrants receive the coordination benefit, while their parents' linguistic heritage is the transferred cost.
% ABSENT_VOICES: First-generation immigrant parents whose native languages were suppressed had no formal voice in the language-planning bodies that set school and settlement policy; their objections survive mainly in memoir and oral history rather than institutional record, and the standard itself defines their objection as diasporic nostalgia rather than legitimate grievance.
% DISAPPEARANCE_RATIONALE: If the native-acquisition standard were abandoned as the criterion for linguistic life, the entire evidentiary basis for calling Hebrew 'revived' rather than 'reconstructed as a liturgical-plus-auxiliary language' would collapse, undermining the specific national narrative that Jewish peoplehood was organically restored; multilingual coexistence models (as with Yiddish or Ladino surviving alongside Hebrew) would become historically live counterfactuals rather than settled failures.
% FOUNDING_PROBLEM: A population of Jewish immigrants from dozens of linguistic backgrounds needed one shared vernacular to function as a cohesive national society, and Hebrew — the shared liturgical and literary language across all these communities — was chosen and deliberately re-engineered into a native mother tongue for the purpose.
% FOUNDING_PROBLEM_CORROBORATION: Linguists external to Zionist institutions (e.g., historians of language revival, sociolinguists studying language shift) corroborate that the coordination problem was real and that Hebrew nativization is the only documented case of full language revival — but the same external scholarship also documents, independently of Zionist institutional sources, that the coordination was achieved through active suppression of Yiddish, Ladino, and Judeo-Arabic rather than through voluntary displacement, a finding not attested by the benefiting national institutions themselves.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.15 (1880, revival just beginning, low institutional coercion) to 0.62 (1948, statehood approaching, full institutional apparatus of Hebrew-only schooling and social sanction in place) because the coordination benefit to the sabra generation is real but was purchased by escalating suppression of parallel home languages, not by voluntary convergence alone. Suppression climbs faster and higher (0.35 to 0.71) than extractiveness because active measures — school exclusion, social shaming, employment incentives tied to Hebrew fluency — were the actual mechanism generating native acquisition, not incidental to it. Theater ratio stays low throughout (0.08 to 0.22) because the underlying function — genuinely producing native Hebrew speakers where none existed — was real and largely achieved, not merely performed; this is a case of substantive coordination riding on substantive extraction, which is exactly the tangled-rope signature rather than a snare or a pure rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Sabra children and the national movement sit near the beneficiary end: children receive a functioning native language and social belonging without having chosen the environment engineered for them; the movement receives the demonstrable evidence of organic peoplehood it needed for its broader legitimacy claims. Immigrant parents speaking Yiddish, Ladino, and Judeo-Arabic sit near the full-target end: trapped by the same demographic and political circumstances that brought them to Palestine, they had no meaningful exit from a schooling and social system engineered to make their children native Hebrew speakers instead of native speakers of the parents' own languages — their d is high because the constraint's operation directly converts their household linguistic capital into loss with no compensating benefit to them personally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating a linguistically fragmented immigrant population into a functioning national society — was genuinely live during the Mandate period and is not obviously dead even now (Israeli society remains linguistically Hebrew-centered by design). This prevents mislabeling the whole arrangement as pure extraction: there was and is a real coordination function. But the specific costs imposed on Yiddish, Ladino, and Judeo-Arabic speakers were not incidental externalities of an otherwise neutral process — they were the mechanism. Classifying this as tangled_rope rather than snare or rope preserves both halves: genuine coordination achievement AND asymmetric extraction from specific victim populations, held together by active enforcement (school policy, social sanction) that would need to persist to maintain the outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_period_reality,
    'Was Hebrew genuinely linguistically ''dead'' from 70-1880 CE under any coherent definition, or does the native-acquisition criterion simply define a dormant-but-living language out of existence by fiat?',
    'Comparative sociolinguistic analysis of other languages with continuous liturgical/literary use but no native speakers (e.g., Sanskrit, Latin, Coptic) against languages that maintained native-speaker chains through diaspora (e.g., Romani); establishing whether ''dead'' is a coherent category independent of the criterion chosen to measure it.',
    'If the dormancy claim is an artifact of the native-acquisition criterion rather than an independently verifiable fact, this reading''s central historical claim (Hebrew was dead, then revived) is itself observer-relative in a way that undercuts the ε-invariance of the ''revival'' framing — though not this story''s ε, which is authored for this reading''s own lights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_period_reality, conceptual, 'Whether Hebrew''s pre-1880 status as ''dead'' is a fact independent of the chosen liveness criterion, or a definitional artifact of this specific reading.').

omega_variable(
    victim_set_disputability,
    'Were Yiddish, Ladino, and Judeo-Arabic speakers coerced into language shift by deliberate institutional suppression, or did they voluntarily converge on Hebrew as the superior coordination equilibrium given the practical benefits of a shared national language?',
    'Archival analysis of Yishuv-era school enrollment policy, language-committee correspondence, and oral history testimony from first-generation immigrants regarding social and institutional sanctions for home-language use.',
    'If shift was substantially voluntary, extractiveness and the victim declaration should be revised sharply downward, moving this reading toward rope; if substantially coerced (as authored here), tangled_rope with a genuine victim class is the accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_disputability, empirical, 'Whether the language shift imposed on diaspora vernacular speakers was coercive or substantially voluntary.').

omega_variable(
    kernel_framing_dependence,
    'Does the choice of native-generational reading over the liturgical-preservation or marketplace-pidgin readings track an independent fact about what ''linguistic life'' means, or does it track which reading best serves Zionist national-legitimacy narratives?',
    'Cross-cultural comparison of how other diaspora or minority-language communities define linguistic vitality (e.g., UNESCO endangered-language criteria, which use intergenerational transmission as one of several factors, not a single decisive test), to assess whether the native-acquisition-only criterion is a defensible sociolinguistic standard or a criterion selected because it uniquely validates the Hebrew revival as an unprecedented achievement.',
    'If the criterion selection is itself motivated by the national-legitimacy payoff, then this reading''s claimed_type and beneficiary structure should be read as partly self-serving rather than a neutral linguistic fact, strengthening the case for treating the coordination claim skeptically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_dependence, conceptual, 'Whether the native-acquisition criterion for linguistic life is a neutral sociolinguistic standard or one selected for its favorable fit with Zionist national narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(hebr_tr_t1897, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(hebr_tr_t1910, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1910, 0.13).
narrative_ontology:measurement(hebr_tr_t1922, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1922, 0.17).
narrative_ontology:measurement(hebr_tr_t1936, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1936, 0.2).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(hebr_be_t1897, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1897, 0.28).
narrative_ontology:measurement(hebr_be_t1910, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1910, 0.42).
narrative_ontology:measurement(hebr_be_t1922, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1922, 0.51).
narrative_ontology:measurement(hebr_be_t1936, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1936, 0.58).
narrative_ontology:measurement(hebr_be_t1948, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1948, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement(hebr_su_t1897, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1897, 0.48).
narrative_ontology:measurement(hebr_su_t1910, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1910, 0.6).
narrative_ontology:measurement(hebr_su_t1922, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1922, 0.66).
narrative_ontology:measurement(hebr_su_t1936, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1936, 0.7).
narrative_ontology:measurement(hebr_su_t1948, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1948, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_linguistic_life kernel, decomposed per the epsilon-invariance principle because 'is Hebrew alive' evaluates to structurally different claims with different epsilon values depending on which liveness criterion is applied. The liturgical_preservation_reading treats continuous sacred-text transmission as sufficient for life and would author near-zero extraction (Hebrew was never dead, no revival extraction occurred). The marketplace_pidgin_reading treats inter-communal practical function as sufficient and would likewise deny the 70-1880 dormancy period, authoring low extraction from a different angle (trade/prayer-house Hebrew was always 'alive' in this reading's terms). Only the native_generational_reading authored here treats the dormancy period as real, requires an active revival, and identifies a victim class in the diaspora vernaculars displaced by that revival — hence the substantially higher epsilon (0.62) authored in this file relative to what the sibling readings would author for the same historical period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
