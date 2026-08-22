% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Marketplace-Pidgin Vitality Standard for Pre-1880 Jerusalem Hebrew
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates the marketplace-pidgin reading of the
 *   contested 'Hebrew linguistic life' kernel. On this reading, the modified
 *   Medieval Hebrew pidgin used daily in pre-1880 Jerusalem markets for
 *   inter-communal trade already constituted Hebrew as a living language —
 *   functioning practically across communities with no other shared tongue —
 *   independent of whether it had native mother-tongue speakers or served a
 *   sacred liturgical function. This is one of three sibling readings of the
 *   same kernel (liturgical continuity; native generational acquisition);
 *   each is authored as its own constraint with its own ε, beneficiaries, and
 *   victims per the ε-invariance principle. This story does not adjudicate
 *   between the readings or average across them — it authors the
 *   marketplace-pidgin claim cleanly.
 *
 * KEY AGENTS:
 *   - jerusalem_marketplace_traders: primary beneficiaries — the pidgin is their working tool
 *   - sephardi_ashkenazi_intercommunal_brokers: agenda-setters who shape and maintain shared usage
 *   - pilgrim_and_diaspora_visitors: secondary beneficiaries with low entry cost via prior liturgical literacy
 *   - rabbinic_liturgical_authorities: bear reputational/doctrinal cost from the pidgin's counter-evidence to liturgical-only vitality
 *   - later_revivalist_historiographers: bear narrative cost from continuity evidence undercutting the death-and-revival arc
 *   - contemporary_sociolinguists: analytical observers assessing the historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.42).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.55).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Marketplace-Pidgin Vitality Standard for Pre-1880 Jerusalem Hebrew").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '906eefca-9efa-40c9-80f9-fd683b03a37f').
narrative_ontology:cs_kernel_codification('906eefca-9efa-40c9-80f9-fd683b03a37f', distributed).
narrative_ontology:cs_authority_grounding('906eefca-9efa-40c9-80f9-fd683b03a37f', distributed).
narrative_ontology:cs_reading_relation('906eefca-9efa-40c9-80f9-fd683b03a37f', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('906eefca-9efa-40c9-80f9-fd683b03a37f', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('906eefca-9efa-40c9-80f9-fd683b03a37f', foundational, practical_function_suffices_for_vitality).
narrative_ontology:cs_axiom_status(practical_function_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('906eefca-9efa-40c9-80f9-fd683b03a37f', practical_function_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('906eefca-9efa-40c9-80f9-fd683b03a37f', secondary, sacred_function_not_required_for_life).
narrative_ontology:cs_axiom_status(sacred_function_not_required_for_life, holdable).
narrative_ontology:cs_axiom_grounding('906eefca-9efa-40c9-80f9-fd683b03a37f', sacred_function_not_required_for_life, conventional).
narrative_ontology:cs_reference_frame('906eefca-9efa-40c9-80f9-fd683b03a37f', ottoman_era_multicommunal_marketplace_practice).
narrative_ontology:cs_drift_state('906eefca-9efa-40c9-80f9-fd683b03a37f', post_1880_revivalist_nationalist_historiography, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('906eefca-9efa-40c9-80f9-fd683b03a37f', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_marketplace_traders).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intercommunal_brokers).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, pilgrim_and_diaspora_visitors).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, rabbinic_liturgical_authorities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, later_revivalist_historiographers).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, functional_definition_of_linguistic_life).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, continuous_adaptation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sephardi, Ashkenazi, Maghrebi, and Arabic-speaking traders in the Old City markets use a modified Medieval Hebrew pidgin daily to negotiate prices, settle disputes, and coordinate across communities that share no other common tongue. For them the pidgin is simply the working language of commerce; its adequacy is proven every market day, not argued for.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_marketplace_traders, beneficiary,
    moderate, biographical, constrained, local).

% Serve as go-betweens for communities whose vernaculars (Ladino, Yiddish, Judeo-Arabic) are mutually unintelligible; they actively maintain and adapt the pidgin's shared vocabulary because their livelihood depends on it functioning as a reliable inter-communal channel. They have practical authority over which usages stick, even without formal institutional standing.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intercommunal_brokers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intercommunal_brokers, agenda_setter).

% Visiting Jews from across the diaspora arrive with liturgical Hebrew literacy but no shared vernacular with local residents; the marketplace pidgin lets them transact and communicate immediately upon arrival, using a register their study of prayer texts partly prepared them for. They benefit from the pidgin's low entry threshold without needing native fluency.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, pilgrim_and_diaspora_visitors, beneficiary,
    powerless, immediate, mobile, regional).

% Guardians of the view that Hebrew's vitality resides in unbroken sacred transmission through study and recitation. The marketplace pidgin's casual, commercially-driven, grammatically loose usage is read by some of these authorities as a debasement of the sacred register, diluting the language's prestige and blurring the line between holy tongue and street speech. They bear a reputational and doctrinal cost every time the pidgin is cited as evidence that Hebrew was already 'alive' before any revival project, since this undercuts claims that liturgical continuity alone constitutes life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, rabbinic_liturgical_authorities, payer,
    institutional, generational, constrained, regional).

% Twentieth-century nationalist historians and linguists built a narrative in which Hebrew was essentially dead as a spoken language until deliberate revival (Ben-Yehuda and successors) resurrected it as a native mother tongue. Evidence of a functioning pre-1880 marketplace pidgin complicates this founding narrative, since it shows continuous practical use rather than a clean death-and-resurrection arc; acknowledging the pidgin costs these historiographers some of the revival narrative's rhetorical force.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, later_revivalist_historiographers, payer,
    organized, generational, constrained, continental).

% Study historical language-contact records, travelers' accounts, and market correspondence to assess whether functional inter-communal use constitutes linguistic life independent of native-speaker status or sacred use. Their analysis feeds directly into how this kernel's competing readings are adjudicated in scholarship.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, contemporary_sociolinguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__marketplace_pidgin_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The pidgin solves a genuine practical problem: communities with no shared vernacular (Sephardi, Ashkenazi, Maghrebi, Arabic-speaking, and visiting diaspora Jews) need a common medium to conduct trade, resolve disputes, and coordinate daily commercial life in a shared urban space.
% TRANSFER_FUNCTION: The arrangement does not primarily transfer material resources; it transfers legitimacy and historiographical authority — evidence of the pidgin's functioning shifts credit for Hebrew's 'aliveness' away from liturgical guardians and away from later revival narratives, toward the ordinary marketplace practice of traders and brokers.
% ABSENT_VOICES: The pidgin speakers themselves left few written records defending their usage as linguistically legitimate; their voice survives mainly through travelers' accounts, court records, and incidental correspondence, not through self-authored linguistic argument. They are structurally excluded from the historiographical debate about what counts as their language's life.
% DISAPPEARANCE_RATIONALE: If the marketplace pidgin reading were dropped from scholarly and communal memory, market records would still show the same historical transactions, but the significance attached to them would collapse into whichever sibling reading dominates — liturgical continuity or native revival. Historians and language-status arguments (e.g. in contemporary debates about minority language vitality) would lose a data point; the underlying historical practice itself is unaffected, but its evidentiary use in adjudicating what counts as 'a living language' would disappear.
% FOUNDING_PROBLEM: Multiple Jewish communities with mutually unintelligible vernaculars, plus a steady stream of diaspora visitors and pilgrims, needed a workable shared medium for everyday commercial and civic coordination in Ottoman-era Jerusalem, well before any organized revival movement existed.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman tax and market records, and non-Jewish traveler accounts (e.g. European and Ottoman administrative observers describing marketplace transactions), corroborate that a functioning Hebrew-based pidgin was in active use for commerce prior to 1880 — these sources sit outside both the rabbinic-liturgical and revivalist-nationalist beneficiary sets, since neither group had an interest in documenting casual marketplace usage at the time.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 at interval end) because this reading is not primarily a resource-extraction mechanism but a legitimacy-reallocation mechanism: it shifts credit for Hebrew's vitality away from institutions invested in liturgical or revivalist narratives. Suppression (0.55) reflects real historiographical friction — the marketplace evidence has at times been minimized or omitted from nationalist and liturgical accounts because it complicates their preferred narratives, not because the underlying commercial practice was itself coercively organized. Theater ratio is low-to-moderate and rising slowly (0.15 to 0.28), reflecting increasing performative invocation of the pidgin as a rhetorical trump card in later 20th-century linguistic-nationalism debates, layered on top of what was originally a purely functional commercial practice.
 *
 * PERSPECTIVAL GAP:
 *   From the trader/broker seats, the pidgin simply worked — no elaborate theory needed, no seat experiences it as an imposed constraint at all. From the rabbinic and revivalist seats, the same historical fact operates as an uncomfortable counter-evidence structure that threatens the internal coherence of their own preferred vitality criteria. The engine should compute a much lower effective extraction from the beneficiary seats and a moderate extraction from the payer seats, driven by directionality rather than any change in the underlying historical fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Traders, brokers, and visitors are declared beneficiaries because the pidgin directly serves their coordination needs with essentially no cost imposed on them by this reading being true. Rabbinic authorities and revivalist historiographers are declared victims not because the pidgin harmed them materially but because the marketplace-pidgin reading, if accepted as authoritative, erodes the exclusivity of their preferred vitality criteria (unbroken liturgical transmission; native mother-tongue revival) — a reputational and doctrinal cost concentrated in institutions whose authority partly rests on those criteria being the correct ones.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in both directions: it does not claim the pidgin was merely decorative survival (which would understate its coordination function) nor does it claim the pidgin alone proves continuous fluent vernacular transmission (which would overstate it into the native-generational reading's territory). The founding-problem interview keeps this honest — the founding problem (inter-communal commercial coordination) is corroborated by non-Jewish administrative sources outside either beneficiary set, which is exactly the kind of external corroboration that prevents this reading from being merely a self-serving retrospective narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidgin_vs_creole_boundary,
    'Was the pre-1880 Jerusalem marketplace Hebrew a stable pidgin with fixed grammatical conventions, or a looser ad hoc contact register that varied trader-to-trader and lacked the systematicity needed to count as a genuine linguistic system?',
    'Systematic corpus analysis of surviving market records, court transcripts, and traveler transcriptions for consistent grammatical and lexical patterns across multiple independent sources and time periods.',
    'If the register was too unstable to count as a coherent linguistic system, the marketplace-pidgin reading''s claim to constitute genuine ''linguistic life'' weakens considerably and collapses toward mere code-switching or ad hoc gesture-language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_vs_creole_boundary, empirical, 'Whether the marketplace register had enough systematicity to count as a language rather than improvised contact speech.').

omega_variable(
    functional_definition_contestability,
    'Is ''functions as inter-communal medium for practical coordination'' a defensible sufficient condition for linguistic life, or does it smuggle in an anachronistic functionalist definition that would count many pidgins and trade jargons as ''living languages'' in a way most linguists would reject?',
    'Comparative analysis against established sociolinguistic vitality frameworks (e.g. UNESCO language vitality factors, Fishman''s GIDS scale) to see whether marketplace-pidgin status alone satisfies recognized vitality criteria elsewhere.',
    'If the functional definition is too permissive, this reading''s claim of ''aliveness'' becomes a definitional artifact rather than a substantive historical finding, weakening its standing relative to the liturgical and native-generational siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_definition_contestability, conceptual, 'Whether the functional-medium definition of linguistic life is itself contestable rather than a neutral empirical criterion.').

omega_variable(
    sources_selection_bias,
    'Do surviving records of the marketplace pidgin over-represent commercially notable interactions (disputes, tax records) while under-representing routine daily use, biasing the historical picture toward exceptional rather than typical usage?',
    'Systematic review of the full range of surviving Ottoman-era Jerusalem archival material, weighting for document type and occasion, to assess whether the pidgin''s use was pervasive daily practice or an occasional expedient.',
    'If usage was rare or occasional rather than pervasive, the marketplace-pidgin reading''s claim to represent an ''alive'' functioning medium (rather than an occasional expedient) is substantially weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sources_selection_bias, empirical, 'Whether surviving evidence reflects pervasive daily pidgin use or only exceptional documented incidents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(hebr_tr_t80, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(hebr_be_t80, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 100, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_linguistic_life__marketplace_pidgin_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_linguistic_life kernel, each authored as a separate story per the ε-invariance principle: liturgical_preservation_reading (ε keyed to continuity of sacred recitation, largely indifferent to vernacular status), marketplace_pidgin_reading (this story; ε keyed to functional inter-communal coordination), and native_generational_reading (ε keyed to child mother-tongue acquisition across all secular domains). The three do not share an ε — each measures a structurally distinct claim about what constitutes linguistic vitality, and each has its own beneficiary/victim structure. They are linked here via network edges rather than merged into one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
