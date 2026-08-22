% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew Native-Generational Linguistic Life Criterion
 *   domain: sociolinguistics/nationalism/religious
 *
 * SUMMARY:
 *   The native-generational reading of Hebrew linguistic life insists that a
 *   language is 'alive' ONLY when children acquire it as mother tongue and
 *   use it for all daily functions including secular mundane speech. This
 *   reading emerged through Eliezer Ben-Yehuda and the Hebrew revival
 *   movement (1880s onward) as a deliberate response to diaspora linguistic
 *   fragmentation. Unlike the liturgical-preservation reading (which held
 *   Hebrew alive through continuous sacred study despite vernacular dormancy)
 *   or the marketplace-pidgin reading (which measured aliveness by functional
 *   coordination regardless of native speaker status), the
 *   native-generational reading centers authenticity and nativity as the
 *   criterion for linguistic life. This reading drove institutional
 *   suppression of Yiddish, Ladino, and other Jewish diaspora languages,
 *   coercing language shift toward Hebrew in educational and state
 *   institutions. The constraint extracted from diaspora linguistic
 *   minorities the cost of language abandonment and cultural assimilation.
 *   The kernel is contested: three readings offer fundamentally different
 *   measures of what 'alive' means, and each reading produces different
 *   victim/beneficiary structures and different measured extractiveness.
 *
 * KEY AGENTS:
 *   - hebrew_revivalists: organized agenda-setters (identity_locked) who formalized the native-generational criterion and embedded it in institutions; benefited from ideological authority over Jewish nationalism
 *   - yiddish_speakers: organized payers (constrained exit) with 11+ million speakers; bore the cost of coerced language shift and delegitimacy under the native-generational reading
 *   - ladino_speakers: organized payers (constrained exit) across Mediterranean/Balkan communities; faced language erosion and institutional pressure toward Hebrew adoption
 *   - hebrew_linguistic_authorities: institutional agenda-setters (identity_locked) who implemented the native-generational criterion through schools and state policy; benefited from institutional power
 *   - religious_authorities_traditional: organized but excluded; held the liturgical-preservation reading and contested the secular native-generational reading; their voice was structurally absent from nation-building discourse
 *   - diaspora_linguistic_minorities: powerless payers (identity_locked) with Judeo-Arabic, Judeo-Persian, Judeo-Greek, and other Jewish vernaculars; bore maximum extraction and assimilation pressure
 *   - contemporary_observers_sociolinguists: analytical observers who measure language vitality by multiple criteria and observe the native-generational criterion as one historically contingent reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.76).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.81).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Native-Generational Linguistic Life Criterion").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/nationalism/religious").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '50ca2e2c-1787-42d8-8be4-551340fcaf31').
narrative_ontology:cs_kernel_codification('50ca2e2c-1787-42d8-8be4-551340fcaf31', distributed).
narrative_ontology:cs_authority_grounding('50ca2e2c-1787-42d8-8be4-551340fcaf31', extraction).
narrative_ontology:cs_interpretation_layer_present('50ca2e2c-1787-42d8-8be4-551340fcaf31').
narrative_ontology:cs_reading_relation('50ca2e2c-1787-42d8-8be4-551340fcaf31', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('50ca2e2c-1787-42d8-8be4-551340fcaf31', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('50ca2e2c-1787-42d8-8be4-551340fcaf31', foundational, native_speaker_nativity_criterion).
narrative_ontology:cs_axiom_status(native_speaker_nativity_criterion, holdable).
narrative_ontology:cs_axiom_grounding('50ca2e2c-1787-42d8-8be4-551340fcaf31', native_speaker_nativity_criterion, deontological).
narrative_ontology:cs_axiom('50ca2e2c-1787-42d8-8be4-551340fcaf31', foundational, secular_functionality_requirement).
narrative_ontology:cs_axiom_status(secular_functionality_requirement, holdable).
narrative_ontology:cs_axiom_grounding('50ca2e2c-1787-42d8-8be4-551340fcaf31', secular_functionality_requirement, deontological).
narrative_ontology:cs_reference_frame('50ca2e2c-1787-42d8-8be4-551340fcaf31', hebrew_authentic_national_revival).
narrative_ontology:cs_drift_state('50ca2e2c-1787-42d8-8be4-551340fcaf31', contemporary_multilingual_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('50ca2e2c-1787-42d8-8be4-551340fcaf31', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, jewish_nation_builders).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, diaspora_linguistic_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, secular_jewish_nation_builders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Eliezer Ben-Yehuda and the Hebrew revival movement formalized the native-generational criterion: Hebrew is alive only when children acquire it as mother tongue and use it for secular daily functions. They created schools enforcing Hebrew immersion, suppressed competing vernaculars in education, and defined linguistic authenticity around native speaker status. They controlled institutional power in the Yishuv and later Israel, implementing the native-generational reading through state policy, curriculum, and media standardization. Their professional identity, authority, and legacy depend on the success of Hebrew revival as defined by this reading.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists, agenda_setter,
    organized, generational, identity_locked, global).

% Spoke Yiddish as mother tongue across Eastern Europe with 11+ million speakers by 1930. The native-generational criterion rendered Yiddish linguistically 'dead' despite active use, cultural vitality, and robust intergenerational transmission. Educational institutions in the Yishuv and Israel shifted to Hebrew immersion; social and institutional pressure coerced language shift. Children were forbidden to speak Yiddish in schools; Yiddish became stigmatized as inauthentic, diaspora, or backward. Their linguistic heritage was reframed as linguistically 'dead' and culturally inauthentic. They retained functional use in family and community but faced maximum institutional suppression of intergenerational transmission.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    organized, biographical, constrained, continental).

% Spoke Judeo-Spanish (Ladino) across Mediterranean, Balkans, and Ottoman communities with continuous cultural transmission and literary tradition. The native-generational criterion rendered Ladino 'linguistically dead' despite active use. Levantine Jewish communities faced institutional and ideological pressure to adopt Hebrew as marker of Jewish national identity. Schools shifted to Hebrew; media standardized Hebrew; social pressure against Ladino use accelerated. Intergenerational transmission was disrupted; Ladino became stigmatized as diaspora language. They experienced coerced language shift from Ladino to Hebrew as the price of belonging to the Jewish nation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    moderate, biographical, constrained, continental).

% Academic and state institutions in the Yishuv and later Israel embedded the native-generational criterion into language planning, educational curriculum, and national identity policy. Authorities controlled schools, publishing, media, and official language standards. The native-generational reading became the legal and cultural standard for measuring linguistic 'aliveness.' Institutional enforcement through education, media, and official language policy defended the criterion. Authorities were identity-locked to the reading because institutional legitimacy and professional authority depended on the success of Hebrew as defined by the native-generational standard.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Orthodox religious authorities held the liturgical-preservation reading: Hebrew was alive through continuous sacred study and transmission regardless of vernacular use. This reading contested the secular native-generational criterion as insufficiently grounded in sacred tradition. They argued Hebrew's aliveness was defined by the unbroken chain of Talmudic study and liturgical recitation, not by the secular measure of native speakers in mundane speech. Their voice was structurally excluded from the Zionist nation-building institutions that adopted the native-generational reading. They saw the native-generational reading as a secular usurpation of religious linguistic authority.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, religious_authorities_traditional, excluded,
    organized, civilizational, constrained, global).

% Speakers of Judeo-Arabic, Judeo-Persian, Judeo-Greek, and other Jewish vernaculars faced the same delegitimacy and coercion under the native-generational criterion. Each diaspora language was rendered 'dead' or 'inauthentic' despite active use and cultural meaning. Institutional pressure toward Hebrew adoption was maximum for these powerless minorities. They lacked organizational power to resist. Intergenerational transmission of their languages was severely disrupted through institutional enforcement and social stigma. They bore the maximum extraction cost of the native-generational reading because they had fewest resources to resist language shift.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, diaspora_linguistic_minorities, payer,
    powerless, biographical, identity_locked, global).

% Used the native-generational criterion to anchor Jewish national identity in secular linguistic authenticity rather than religious tradition. Hebrew as native mother tongue became the defining marker of 'true' Jewish belonging and national authenticity. They collected legitimacy for the Zionist nation-building project by making Hebrew linguistic revival a proxy for national authenticity and modernism. The native-generational reading provided secular grounding for Jewish nationalism: nationality = native language speaker community. They benefited from the ideological authority and institutional power the reading conferred.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, secular_jewish_nation_builders, beneficiary,
    institutional, generational, identity_locked, national).

% Held the marketplace-pidgin reading: Hebrew functioned effectively as inter-communal medium for practical coordination across linguistic communities. This reading de-centered native speaker status and sacred function; aliveness was measured by utility in coordination. Pragmatist traders in diaspora communities used Hebrew for inter-community commerce and coordination without requiring native speaker status. The pragmatist reading was structurally excluded from nation-building discourse that prioritized ideological authenticity over functional coordination. Their reading contested the necessity of the native-generational criterion.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, pragmatist_pidgin_traders, excluded,
    moderate, immediate, mobile, regional).

% Measure language vitality by multiple criteria: number of native speakers, intergenerational transmission, functional domains, speaker attitudes, institutional support, language prestige, media representation. From this analytical seat, the native-generational criterion is ONE possible measure among many defensible alternatives. Sociolinguists observe that Hebrew functions effectively in many domains; diaspora languages maintain vitality despite suppression; multiple readings of 'linguistic aliveness' are empirically coherent. They observe the constraint as a historically contingent reading that extracted substantial costs from competing linguistic communities without necessity. Their role is to measure rather than advocate, but measurement reveals the reading's contingency and extractiveness.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, contemporary_observers_sociolinguists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_authorities).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified linguistic commons for a dispersed diaspora Jewish population lacking a shared vernacular after 70 CE. Multiple Jewish communities across geographies spoke mutually incomprehensible languages (Yiddish, Ladino, Judeo-Arabic, etc.) and lacked coordination mechanism. Hebrew revival solved this by creating a shared language: one linguistic standard for all Jews regardless of diaspora origin. Enables cultural reunion, national organization, and common identity across diaspora communities that had evolved separate linguistic traditions over centuries.
% TRANSFER_FUNCTION: Transfers linguistic authority and cultural legitimacy from diaspora vernaculars (Yiddish, Ladino, Judeo-Arabic, etc.) to Hebrew. Enforces language shift among diaspora speakers through educational institutions, state policy, media standardization, and social pressure. Subordinates competing Jewish linguistic heritages (each with centuries of cultural transmission) to a single national language standard. Extracts from diaspora linguistic minorities the cost of language abandonment, loss of intergenerational transmission, cultural assimilation, and linguistic stigmatization.
% ABSENT_VOICES: Religious authorities holding the liturgical-preservation reading are structurally excluded from nation-building institutions that adopt the native-generational criterion. Their voice would contest the secular definition of linguistic aliveness and argue for religious-grounded criteria. Pragmatist merchants and traders who used Hebrew functionally in marketplace settings are excluded — their reading contests the centrality of native speaker status and offers an alternative measure of aliveness. Diaspora linguistic minorities (Yiddish, Ladino, Judeo-Arabic speakers) have lived experience of the constraint's extractive force and could attest to the costs of forced language shift, but lack institutional power to contest the national narrative in dominant institutions.
% DISAPPEARANCE_RATIONALE: If the native-generational criterion disappeared, Hebrew could continue functioning as lingua franca and coordination mechanism under the liturgical or marketplace readings. Diaspora speakers would not face coerced language shift and could maintain intergenerational transmission of competing vernaculars. Multiple Jewish linguistic communities could coexist. Educational and state institutions would reorganize around different language-vitality measures. The Jewish diaspora linguistic ecology would restructure from monolingually Hebrew-centric back to plurilingual communities. Publishing, media, and cultural institutions would accommodate multiple Jewish languages rather than enforcing Hebrew monolingualism.
% FOUNDING_PROBLEM: Diaspora Jewry lacked a shared vernacular for national coordination and cultural reunion after 70 CE dispersal period. Multiple communities across geographies spoke mutually incomprehensible languages (Yiddish in Eastern Europe, Ladino in Mediterranean/Balkans, Judeo-Arabic in Middle East, Judeo-Persian in Iran, etc.). These linguistic communities had evolved separately over centuries without unified communication system. A common language was needed to anchor emerging Jewish nationalism in the 19th-20th centuries and enable practical coordination across diaspora communities seeking political and cultural reunion. The founding problem: diaspora fragmentation requires linguistic unification.
% FOUNDING_PROBLEM_CORROBORATION: Hebrew revivalists and Zionist nation-builders attest the problem was real and critical: diaspora fragmentation threatened Jewish national coherence and revival required a shared language. Contemporary sociolinguists and diaspora historians, however, attest the founding problem admits multiple solutions and the native-generational reading was ideological choice, not necessity. They observe Hebrew functioned as marketplace lingua franca and liturgical standard BEFORE the native-generational revival was formalized in the 1880s — coordination was partially achievable through existing functions. Some scholars argue the problem could have been solved through multilingual cooperation and plurilingual education rather than through linguistic suppression and hegemony. Yiddish scholars attest Yiddish-speaking communities had solved the intra-diaspora coordination problem through Yiddish literature, press, and culture without requiring language shift to Hebrew. The founding problem (diaspora coordination) is real but the native-generational solution was one choice among viable alternatives.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.76, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.76 at interval end) because the native-generational reading delegitimizes all competing Jewish vernaculars and coerces their abandonment through institutional pressure. The constraint is not natural law — it is a constructed criterion that benefits Hebrew-centric nation-builders and extracts from diaspora linguistic minorities. Suppression is higher still (0.81) because maintaining the native-generational criterion requires actively preventing intergenerational transmission of competing languages and enforcing Hebrew linguistic purism in schools, state institutions, and media. Theater ratio is substantial (0.42): much of the institutional enforcement activity is now performative — linguistic policing continues as identity ritual even though the functional coordination problem is solved and Hebrew native-speaker acquisition is automatic. The measurement series shows clear acceleration of extraction, suppression, and theater during 1875-1950 (the Yishuv and statehood period) followed by stabilization at high levels. The three measurement series align on one shared time grid across all time points. The trajectory shows the constraint evolving from a pragmatic coordination solution (1850) into an increasingly extractive, enforced, and performative instrument of national identity (1950-2000).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (hebrew_revivalists, hebrew_linguistic_authorities) compute the constraint as coordination: solving diaspora fragmentation through linguistic unification. From this seat, the native-generational criterion is the authentic measure of 'alive' language necessary for nation-building. The payer seats (yiddish_speakers, ladino_speakers, diaspora_linguistic_minorities) compute the constraint as extraction: coercive suppression of their linguistic heritage and forced assimilation to a foreign language standard. The engine computes this divergence from the structural data: beneficiary identity (Hebrew-centric nation-builders), victim identity (diaspora language speakers), and the asymmetry of exit options (nation-builders identity-locked to the reading; minority speakers constrained by educational institutions and state policy). The claimed_type (tangled_rope) reflects the dual structure: genuine coordination function (solving diaspora linguistic fragmentation) paired with asymmetric extraction (coercing minority language abandonment). Religious authorities (excluded) hold a fundamentally different reading of what 'alive' means; sociolinguists (observers) note multiple readings are defensible and measure the native-generational reading as one historically contingent choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The hebrew_revivalists and hebrew_linguistic_authorities are the beneficiaries and agenda-setters: they collect ideological authority, institutional power, and legitimacy for Jewish nationalism from the native-generational reading. Their d-value approaches the beneficiary end (low d, ~0.1-0.2). The yiddish_speakers, ladino_speakers, and diaspora_linguistic_minorities are victims and payers: they bear the cost of language abandonment, cultural assimilation, and delegitimacy. Their d-values approach the target end (high d, ~0.8-0.9). The religious_authorities_traditional and pragmatist_pidgin_traders are excluded: their d-values are undefined (they would object if present but have no formal seat). The contemporary_observers_sociolinguists are analytical observers: their d-value is undefined (they measure from outside). The directionality derivation prioritizes: (1) beneficiary/victim declarations (hebrew_revivalists benefit, diaspora speakers victimized), (2) exit options (hebrew_revivalists identity_locked to the reading; diaspora speakers constrained by institutional enforcement), (3) power atoms (organized beneficiaries vs. powerless-to-organized victims). No directionality overrides are needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora linguistic fragmentation; need for shared coordination language) is CONTESTED. Hebrew revivalists and nation-builders attest it remains live: they argue continuous enforcement is necessary to maintain linguistic authenticity and prevent diaspora creolization. Contemporary sociolinguists and diaspora historians attest the founding problem is partially dead: they observe that Hebrew functions as lingua franca and enables coordination WITHOUT requiring the native-generational criterion. Pragmatist readings show marketplace Hebrew achieved the coordination function before the native-generational reading was formalized. The constraint has rotated: the primary function has shifted from solving coordination (1850-1900) toward enforcing national identity purity (1950-2000). Theater ratio rising (0.05 → 0.42) and extraction rising while suppression plateaus (0.81) indicates the constraint is performing identity maintenance more than solving the original coordination problem. This is the pattern of a constraint whose mandate has outlived its function. The native-generational reading should carry mandatrophy_resolved: true, because the founding problem (diaspora linguistic coordination) is solved but the constraint persists as enforcer of ideological purity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_native_generation_criterion,
    'Is the native-generational criterion a natural or inevitable measure of linguistic aliveness, or is it a culturally contingent normative choice?',
    'Comparative analysis of other language revivals (Irish, Basque, Occitan, Marathi) and their criteria for linguistic vitality. If multiple revivals adopt different criteria without universal convergence, the native-generational criterion is contingent; if they all converge on it, it may reflect natural linguistic dynamics.',
    'If contingent: the reading is one interpretive choice among defensible alternatives, strengthening the tangled_rope classification and supporting the mandatrophy finding. If natural: the reading approaches mountain status and the extraction becomes coordination cost rather than rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_native_generation_criterion, conceptual, 'Whether the native-generational criterion is natural linguistic law or constructed normative reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of diaspora languages (0.81) structural (external enforcement through institutions) or internalized (diaspora speakers internalize the stigma and suppress their own language)?',
    'Post-institutional-pressure observation: if diaspora speakers (Yiddish, Ladino) maintain intergenerational transmission when institutional pressure is removed (e.g., cultural reclamation movements, community education), the suppression was structural; if transmission remains suppressed even after institutional pressure ceases, suppression is partially internalized.',
    'If structural: the suppression is external coercion and the constraint''s extractiveness is rooted in institutional force. If internalized: the constraint''s extractiveness persists through stigma and cultural internalization even after formal enforcement ends; the constraint becomes more pernicious.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether diaspora language suppression is institutional coercion or internalized cultural hierarchy.').

omega_variable(
    functional_vs_ideological_enforcement,
    'How much of the current enforcement of the native-generational criterion (2000 CE, t=150) is functionally necessary for coordination vs. ideologically performative for national identity maintenance?',
    'Ethnographic study of institutional language policing: if enforcement stops or reduces without coordination breakdown (e.g., multilingual institutions function normally), enforcement is primarily ideological; if coordination visibly degrades, enforcement is functionally necessary.',
    'If primarily ideological: the rising theater_ratio (0.42) is accurate and the constraint is a degraded coordination mechanism (piton candidate) or a snare disguised as rope. If functionally necessary: the theater ratio should be lower and the constraint remains justified rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_ideological_enforcement, empirical, 'Whether contemporary native-generational enforcement is coordination necessity or nation-state ideology.').

omega_variable(
    alternative_solutions_counterfactual,
    'Were competing readings (liturgical-preservation, marketplace-pidgin) viable solutions to the founding coordination problem, or was native-generational revival the only defensible response to diaspora fragmentation?',
    'Historical analysis of Hebrew''s pre-revival functions in diaspora communities: if marketplace Hebrew and liturgical Hebrew already achieved significant coordination and literacy before Ben-Yehuda''s revival, the founding problem had partial solutions; if Hebrew was genuinely dormant and coordination impossible without revival, native-generational revival was necessary.',
    'If alternatives were viable: the native-generational reading was ideological choice, not necessity; extraction from diaspora languages was imposed rather than required. If native-generational was necessary: extraction was the unavoidable cost of solving the real coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_solutions_counterfactual, empirical, 'Whether native-generational revival was the only solution or one choice among viable alternatives.').

omega_variable(
    kernel_reading_identity_fusion,
    'Why do Hebrew speakers identity-lock to the native-generational reading rather than holding it as one defensible position among competing readings?',
    'Ethnographic study of Hebrew revivalist identity: if Hebrew speakers can articulate the liturgical and marketplace readings as logically coherent alternatives, identity fusion is chosen (not inevitable); if they cannot articulate alternatives or dismiss them as obviously false, fusion is internalized.',
    'If chosen: the agenda-setters are strategically locked and can in principle change reading by changing strategic commitments. If internalized: the constraint operates through cognitive capture and is more pernicious.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity_fusion, conceptual, 'Whether Hebrew speakers'' adherence to native-generational reading reflects strategic identity-lock or internalized cognitive fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__native_generational_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t25, hebrew_linguistic_life__native_generational_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(hebr_tr_t50, hebrew_linguistic_life__native_generational_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(hebr_tr_t75, hebrew_linguistic_life__native_generational_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__native_generational_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(hebr_tr_t125, hebrew_linguistic_life__native_generational_reading, theater_ratio, 125, 0.42).
narrative_ontology:measurement(hebr_tr_t150, hebrew_linguistic_life__native_generational_reading, theater_ratio, 150, 0.42).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t25, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(hebr_be_t50, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(hebr_be_t75, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement(hebr_be_t125, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 125, 0.76).
narrative_ontology:measurement(hebr_be_t150, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 150, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(hebr_su_t25, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(hebr_su_t50, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(hebr_su_t75, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(hebr_su_t100, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 100, 0.81).
narrative_ontology:measurement(hebr_su_t125, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 125, 0.8).
narrative_ontology:measurement(hebr_su_t150, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 150, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, yiddish_language_vitality).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, ladino_language_vitality).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, jewish_diaspora_linguistic_pluralism).

% DUAL FORMULATION NOTE:
% This story is ONE READING of the contested kernel 'hebrew_linguistic_life'. The native-generational reading competes with two sibling readings: (1) liturgical_preservation_reading measures aliveness through continuous sacred transmission; (2) marketplace_pidgin_reading measures aliveness through functional coordination. Each reading produces different ε values and different victim/beneficiary structures because each defines 'aliveness' differently. The three readings are NOT alternative measurements of the same constraint — they are alternative interpretations of a contested kernel. All three must be authored as separate constraint stories linked via network.affects_constraints to enable the engine to compute the boundary between coordinated pluralism and extractive hegemony.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
