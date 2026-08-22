% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew as Living Inter-Communal Market Medium (Marketplace Pidgin Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates the marketplace-pidgin reading of the
 *   contested 'Hebrew linguistic life' kernel: the claim that a modified
 *   Medieval Hebrew functioned continuously as a genuine inter-communal
 *   medium in Jerusalem's markets well before the organized nationalist
 *   revival of the 1880s, and that this functional, practical use —
 *   regardless of anyone's native-speaker status and independent of the
 *   language's sacred liturgical role — is sufficient to establish the
 *   language as 'alive.' This is one of three structurally distinct readings
 *   of the same historical kernel. The liturgical-preservation reading
 *   locates life in unbroken sacred recitation regardless of vernacular use;
 *   the native-generational reading denies life status to anything short of
 *   children acquiring the language as mother tongue for all daily functions.
 *   Each reading has a different beneficiary/victim structure and a different
 *   epsilon: this reading's extraction is moderate (0.42) because it
 *   identifies a real coordination structure whose costs fall
 *   disproportionately on vernacular-monolingual speakers, while the
 *   liturgical reading would show near-zero extraction (a mountain-like
 *   sacred continuity with no clear victims) and the native-generational
 *   reading would likely show much higher extraction (denying vitality status
 *   to the pidgin delegitimizes the mixed communities who relied on it, and
 *   privileges the revival project's own historiography).
 *
 * KEY AGENTS:
 *   - jerusalem_merchant_communities: primary beneficiaries of the coordination function
 *   - sephardi_ashkenazi_intermediaries: agenda-setters who extend Hebrew's market register informally
 *   - yiddish_vernacular_speakers and ladino_vernacular_speakers: bear the asymmetric cost of code-switching into a register that is native to no one
 *   - ben_yehuda_revival_project: later beneficiary of the pidgin's existence as legitimating precedent
 *   - philologists_of_hebrew_revival: analytical observers adjudicating the kernel contest itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.42).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.38).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew as Living Inter-Communal Market Medium (Marketplace Pidgin Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'c5640519-e9df-48bc-8696-4ee1339e9768').
narrative_ontology:cs_kernel_codification('c5640519-e9df-48bc-8696-4ee1339e9768', distributed).
narrative_ontology:cs_authority_grounding('c5640519-e9df-48bc-8696-4ee1339e9768', distributed).
narrative_ontology:cs_reading_relation('c5640519-e9df-48bc-8696-4ee1339e9768', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5640519-e9df-48bc-8696-4ee1339e9768', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('c5640519-e9df-48bc-8696-4ee1339e9768', foundational, functional_use_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(functional_use_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('c5640519-e9df-48bc-8696-4ee1339e9768', functional_use_sufficient_for_vitality, conventional).
narrative_ontology:cs_axiom('c5640519-e9df-48bc-8696-4ee1339e9768', secondary, native_acquisition_not_required_for_life).
narrative_ontology:cs_axiom_status(native_acquisition_not_required_for_life, holdable).
narrative_ontology:cs_axiom_grounding('c5640519-e9df-48bc-8696-4ee1339e9768', native_acquisition_not_required_for_life, empirically_contingent).
narrative_ontology:cs_reference_frame('c5640519-e9df-48bc-8696-4ee1339e9768', medieval_hebrew_commercial_register_continuity).
narrative_ontology:cs_drift_state('c5640519-e9df-48bc-8696-4ee1339e9768', post_1880_organized_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5640519-e9df-48bc-8696-4ee1339e9768', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchant_communities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intermediaries).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, ben_yehuda_revival_project).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, ottoman_administrative_intermediaries).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, yiddish_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, ladino_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, arabic_vernacular_traders).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, functional_definition_of_linguistic_vitality).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, continuity_thesis_of_hebrew_revival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ashkenazi, Sephardi, and Mizrahi traders in the Jerusalem markets who lack a shared mother tongue use a modified Medieval Hebrew pidgin to negotiate prices, settle disputes, and coordinate logistics across communal lines. The pidgin is nobody's native tongue but everyone's usable bridge, and using it costs them nothing they would not otherwise spend learning some intermediary language.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchant_communities, beneficiary,
    moderate, generational, constrained, local).

% Rabbinic and communal figures fluent in liturgical Hebrew extend its market and administrative registers informally, brokering deals and disputes between linguistically separated households. They shape which Hebrew forms circulate in commerce by using them repeatedly in visible transactions, without formal institutional backing.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intermediaries, beneficiary,
    moderate, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intermediaries, agenda_setter).

% Later nationalist revivalists point to the pre-existing marketplace pidgin as evidence that Hebrew never fully died, using it to argue their revival is completion rather than resurrection. They benefit from the pidgin's existence as historical legitimating precedent, even though they did not create it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, ben_yehuda_revival_project, beneficiary,
    moderate, generational, mobile, national).

% Local Ottoman-era clerks and tax intermediaries who deal with multiple religious communities use Hebrew as a neutral inter-communal register alongside Arabic and Turkish, gaining a practical tool for administration without needing to master every community's vernacular.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, ottoman_administrative_intermediaries, beneficiary,
    moderate, biographical, constrained, local).

% Ashkenazi households whose actual daily language is Yiddish are pressed to master an additional register, the market pidgin, purely to transact with non-Yiddish speakers. Their own vernacular carries no market-crossing value, so the burden of bilingual competence falls disproportionately on them without reciprocal accommodation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, yiddish_vernacular_speakers, payer,
    powerless, biographical, trapped, local).

% Sephardi households whose daily language is Judeo-Spanish face the same asymmetry as Yiddish speakers: their mother tongue does no work across communal lines, and market participation requires acquiring the pidgin at their own cost in time and effort.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, ladino_vernacular_speakers, payer,
    powerless, biographical, trapped, local).

% Palestinian Arab traders dealing with Jewish communities in mixed markets sometimes pick up market Hebrew phrases for transactional purposes but bear the cost of a second inter-communal register on top of Arabic, which functions as the dominant regional lingua franca in most other contexts.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, arabic_vernacular_traders, payer,
    moderate, biographical, constrained, local).

% Historical linguists examining Ottoman-era Jerusalem correspondence, market records, and rabbinic responsa to determine whether functional inter-communal Hebrew use in commerce constitutes linguistic life independent of native acquisition or liturgical function.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, philologists_of_hebrew_revival, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, religiously legitimate, communally neutral register that lets Ashkenazi, Sephardi, Mizrahi, and sometimes Arab traders in Jerusalem transact and coordinate without any party's home vernacular being privileged over another's.
% TRANSFER_FUNCTION: Moves the cost of bilingual/multilingual competence onto those whose native vernaculars (Yiddish, Ladino, spoken Arabic) carry no inter-communal market value, while crediting the pidgin's mere existence to nationalist revival narratives that did not create it.
% ABSENT_VOICES: Ordinary Yiddish- and Ladino-speaking households whose vernaculars are functionally sidelined in the market register are rarely recorded as commentators on the pidgin's status; their labor of code-switching is invisible in the historical record that instead documents the pidgin's existence, not its cost.
% DISAPPEARANCE_RATIONALE: If the marketplace pidgin had vanished before 1880, communal groups would likely have found another lingua franca (Arabic, Ottoman Turkish, or trade jargon) for practical coordination — the SPECIFIC linguistic vehicle is replaceable, but the underlying coordination NEED is not, so whether 'the world rearranges' depends on whether you track the function or the specific linguistic form; the two other kernel readings resolve this differently.
% FOUNDING_PROBLEM: Multiple diaspora Jewish communities converging on Ottoman Jerusalem lacked a shared vernacular; something was needed to let merchants, rabbis, and administrators from different linguistic backgrounds transact and coordinate without institutionally privileging any one community's home language.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman tax and market records and rabbinic responsa (external administrative sources, not authored by either the pidgin's users for self-promotion or by later nationalist revivalists) attest to functional Hebrew use in commercial contexts pre-1880; however, later nationalist historiography (Ben-Yehuda's own memoirs and successor accounts) has an interest in reading this pidgin as proof of unbroken vitality, so corroboration from disinterested Ottoman administrative sources is weighted more heavily here than from the revival project's own self-narration.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).
:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate rather than high because the coordination function is genuine and the costs, while real and asymmetric, are not coercively imposed by any single controlling party — no institution enforces use of the pidgin, and vernacular speakers retain other coordination options (translators, Arabic, Ottoman Turkish) even if costlier. Suppression is similarly moderate: there is no active suppression mechanism criminalizing Yiddish or Ladino, but there is a structural pressure that renders those vernaculars functionally invisible in inter-communal commerce, which the resistance score (0.5) reflects as a real but non-violent friction. Accessibility collapse is moderate-low (0.35) because alternative coordination media (other lingua francas, translators, kin networks) persisted throughout the period; this was never the sole option.
 *
 * DIRECTIONALITY LOGIC:
 *   Merchant communities, intermediaries, and later revivalists are coded as beneficiaries because the pidgin either directly serves their transactional needs or retroactively serves their nation-building narrative, in both cases at negligible cost to them (the intermediaries were often already fluent in liturgical Hebrew, so extending it cost little). Yiddish and Ladino vernacular speakers are coded as payers/targets because their actual mother tongues are functionally excluded from the inter-communal register, forcing them to invest in a second linguistic system that is native to nobody — this is a real, if diffuse, transfer of cognitive and social labor. Arabic-speaking traders occupy a middle position: moderate power because Arabic remains regionally dominant, but still bearing some marginal cost when Hebrew intrudes into mixed-market contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   The marketplace-pidgin reading resists the mandatrophy trap of treating linguistic vitality as binary (either fully alive via native transmission, or dead except for sacred use). By defining life functionally, it captures a real intermediate case: a language doing real coordination work without being anyone's mother tongue and without depending on liturgical unbrokenness. This prevents the native-generational reading's tendency to erase the pidgin's real historical function, while also preventing the liturgical reading's tendency to credit sacred continuity for something that was actually a distinct, adaptive market phenomenon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidgin_vs_full_language_boundary,
    'Is the pre-1880 Jerusalem market Hebrew a genuine functioning language (however reduced in register) or merely a set of fixed liturgical/commercial formulae repeated without generative grammar — i.e., was it structurally a pidgin at all, or closer to a limited-domain jargon?',
    'Corpus-linguistic analysis of surviving Ottoman-era Hebrew market correspondence and responsa for evidence of productive grammatical innovation versus fixed formulaic reuse.',
    'If it was a true adaptive pidgin with generative capacity, the marketplace-pidgin reading is on strong structural footing; if it was merely fixed formulae, the reading''s claim to ''linguistic life'' independent of native acquisition weakens considerably and shifts weight toward the native-generational reading''s skepticism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_vs_full_language_boundary, empirical, 'Whether the market Hebrew was a generative pidgin or a fixed formulaic jargon.').

omega_variable(
    committer_framing_kernel_contest,
    'Is the ''aliveness'' of a language best located in functional inter-communal use (this reading), unbroken sacred transmission (liturgical_preservation_reading), or native intergenerational mother-tongue transmission (native_generational_reading) — and is this a factual dispute about linguistics or a values dispute about what ''life'' should mean for a language?',
    'No empirical resolution mechanism fully closes this; it is partly a conceptual dispute about the definition of linguistic vitality that different linguistic and religious traditions resolve differently. Comparative sociolinguistic literature on language revitalization (e.g., debates over Cornish, Manx) could inform but not settle the framing choice.',
    'Choosing this reading over the siblings determines which population counts as a ''victim'' of exclusion (vernacular monolinguals here; potentially no one under the liturgical reading; the revival project''s opponents under the native-generational reading) and which population is credited with keeping Hebrew alive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_kernel_contest, conceptual, 'Which of three incompatible framings of linguistic vitality should govern the kernel, and whether this is an empirical or normative question.').

omega_variable(
    revival_project_appropriation_degree,
    'To what extent did the organized Hebrew revival movement (Ben-Yehuda and successors) genuinely build upon and extend the pre-existing marketplace pidgin, versus constructing a origin myth that overstates continuity to legitimate a largely novel nationalist linguistic project?',
    'Comparative lexical and grammatical analysis of pre-1880 market Hebrew records against early Ben-Yehuda-era revived Hebrew to measure actual continuity versus rupture.',
    'High continuity supports crediting the pidgin as this reading claims; high rupture suggests the revival project''s appropriation of the pidgin as legitimating precedent is itself a form of extraction from the historical record, deepening rather than resolving the marketplace_pidgin_reading''s already-moderate extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_project_appropriation_degree, empirical, 'Degree of genuine linguistic continuity between the market pidgin and organized 20th-century Hebrew revival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t10, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(hebr_tr_t30, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(hebr_tr_t50, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t10, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(hebr_be_t30, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(hebr_be_t50, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 60, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_linguistic_life__marketplace_pidgin_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'Hebrew linguistic life/vitality' per the epsilon-invariance principle. The liturgical_preservation_reading locates vitality in unbroken sacred textual transmission (near-mountain, minimal extraction, essentially no identifiable victims). The native_generational_reading locates vitality strictly in children's mother-tongue acquisition for all daily functions (likely higher extraction, since it delegitimizes both the pidgin users and, implicitly, competing claims to authentic revival). This marketplace_pidgin_reading occupies a moderate-extraction middle position: real coordination function, real but non-coercive asymmetric costs on vernacular-monolingual populations. The three stories are linked bidirectionally via affects_constraints since each reading's public legitimacy affects resource allocation and historiographic authority claimed by the others (e.g., if the pidgin reading gains scholarly acceptance, it structurally weakens the native-generational reading's exclusive claim to define 'life').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
