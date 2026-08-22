% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Trans-Diaspora Hebrew Bridge-Language Regime
 *   domain: sociolinguistic/religious-economic
 *
 * SUMMARY:
 *   From roughly the eleventh century to the eve of the revival (interval
 *   unit = one decade; t0 ~ 1100 CE, t78 ~ 1880 CE), Hebrew functioned
 *   neither as a purely liturgical relic nor as a native vernacular but as
 *   the written and contact medium of a trans-diaspora network: merchants'
 *   ledgers and family letters (preserved en masse in the Cairo Genizah),
 *   responsa crossing every linguistic border, and marketplace registers —
 *   Judeo-German, Judezmo, Judeo-Arabic — carrying dense Hebrew strata inside
 *   vernacular speech. The kernel hebrew_continuity is here occupied through
 *   instrumental utility. The ε referent is the standing arrangement itself —
 *   the Hebrew-medium communication regime as it actually operated, costs and
 *   dividends included — assessed by this reading's own lights; the endorsed
 *   alternatives of the sibling readings are not the referent. Claim and
 *   metrics are authored independently: the tangled_rope claim comes from the
 *   structural analysis (genuine coordination plus asymmetric, actively
 *   enforced extraction); the metric values come from the descriptive record.
 *   The sibling readings' dismissal of this phase as 'not really Hebrew' is
 *   routed to the omega variables, not averaged into ε.
 *
 * KEY AGENTS:
 *   - rabbinic_leadership: agenda-setter and principal beneficiary (institutional / identity_locked) — administers the schooling mandate and collects authority rents from mastered scarcity
 *   - trans_diaspora_merchant_networks: principal commercial beneficiary (powerful / mobile) — consumes the bridge function and retains arbitrage-grade exit into other languages
 *   - heder_students: primary payers (powerless / trapped) — bear the childhood-labor cost of the system
 *   - diaspora_women: excluded payers (powerless / trapped) — fund and sustain the system while being kept outside its literacy
 *   - vernacular_printers_authors: excluded challengers (organized / mobile) — supply the suppressed alternative and absorb the enforcement's edge
 *   - sociolinguistic_analyst: analytical observer — sees the full structure across registers and regions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.58).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.4).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Trans-Diaspora Hebrew Bridge-Language Regime").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistic/religious-economic").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, 'a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2').
narrative_ontology:cs_kernel_codification('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', fixed_text).
narrative_ontology:cs_authority_grounding('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', lineage).
narrative_ontology:cs_interpretation_layer_present('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2').
narrative_ontology:cs_reading_relation('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_axiom('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', foundational, instrumental_occupancy_sustains_language_life).
narrative_ontology:cs_axiom_status(instrumental_occupancy_sustains_language_life, holdable).
narrative_ontology:cs_axiom_grounding('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', instrumental_occupancy_sustains_language_life, empirically_contingent).
narrative_ontology:cs_axiom('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', foundational, register_plurality_constitutes_vitality).
narrative_ontology:cs_axiom_status(register_plurality_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', register_plurality_constitutes_vitality, conventional).
narrative_ontology:cs_reference_frame('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', masoretic_transmission_framework).
narrative_ontology:cs_drift_state('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', haskalah_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a7adddb2-a0fb-48cc-9a11-dd605fe1a6a2', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, trans_diaspora_merchant_networks).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, rabbinic_leadership).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, heder_students).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, diaspora_women).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, lashon_hakodesh_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, diaspora_unity_through_shared_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the curriculum of the heder and yeshiva, issued communal ordinances governing the language of contracts, deeds, and correspondence, and adjudicated disputes through responsa circulated across linguistic borders. Their standing, income, and marriage alliances rested on a mastery scarce enough to be authoritative; stepping outside the Hebrew-medium world would have dissolved the basis of their position, so none of them did.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, rabbinic_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, rabbinic_leadership, beneficiary).

% Ran long-distance trade linking the Mediterranean, the Indian Ocean littoral, and the European interior, keeping ledgers, contracts, and family letters in a Hebrew-script, Hebrew-inflected written register intelligible to counterparts from Fez to Krakow regardless of spoken vernacular. When political or commercial conditions shifted, firms relocated and progressively shifted their correspondence into Ladino, Yiddish, and later the European state languages.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, trans_diaspora_merchant_networks, beneficiary,
    powerful, biographical, mobile, global).

% Entered cheder around age three and spent roughly a decade decoding scripture, memorizing morphology, and translating verse-by-verse into Yiddish or Judeo-Spanish. Most reached working fluency only in narrow written registers; the schooling consumed childhood years and family tuition, and no enrolled child chose the curriculum or could decline it.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, heder_students, payer,
    powerless, biographical, trapped, regional).

% Were taught to read prayers in transliteration or through Yiddish adaptations such as the Tsene-rene rather than the Hebrew text itself, while household earnings helped fund brothers' and sons' schooling. They conducted much of the actual marketplace talk in vernaculars thick with Hebrew loanwords, yet stood outside the literate conversation the schooling maintained and had no institutional channel through which to contest that placement.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_women, excluded,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, diaspora_women, payer).

% Printed Yiddish and later German-language books for the same readership the Hebrew system schooled, and argued in maskilic pamphlets that instruction in the mother tongue would serve the population better. Communal bans on secular reading and proclamations against Enlightenment circles targeted their wares; when local bans bit, presses and authors relocated to Berlin, Vienna, or Odessa and published from there.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, vernacular_printers_authors, excluded,
    organized, biographical, mobile, continental).

% Reads the Genizah letters, the responsa corpora, the school regulations, and the maskilic memoirs as evidence of how the arrangement worked, for whom, and at what cost; holds no stake in any party's position and can set the arrangement beside other multilingual trade regimes when judging it.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, sociolinguistic_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, rabbinic_leadership).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplied a single written medium — Hebrew script over a Hebrew-and-Aramaic lexical core — intelligible across communities whose spoken vernaculars (Yiddish, Judezmo, Judeo-Arabic, Judeo-Persian) were mutually unintelligible, enabling long-distance commerce, legal consultation, and textual study without any shared spoken language.
% TRANSFER_FUNCTION: Moved roughly a decade of each boy's childhood and substantial household resources into literacy training; moved commercial and legal information across the diaspora through Hebrew-channel correspondence; moved interpretive authority and adjudication income to those holding certified mastery.
% ABSENT_VOICES: Women, given vernacular prayer-books in place of the Hebrew text, stood outside the literate conversation their households paid for; vernacular authors and printers, barred from communal respectability, argued for mother-tongue instruction; the enrolled children themselves had no voice in a curriculum selected entirely by others.
% DISAPPEARANCE_RATIONALE: Overnight removal would have severed merchant correspondence chains, halted the responsa traffic that held halakhic practice together across borders, and cut communities off from a canon written almost entirely in Hebrew and Aramaic until substitutes appeared — and when the arrangement did decay, correspondence, adjudication, and reading demonstrably reorganized around European languages, vernacular print, and eventually revived spoken Hebrew.
% FOUNDING_PROBLEM: After Hebrew ceased to be anyone's spoken vernacular, geographically scattered communities speaking unrelated Jewish languages needed one shared medium for law, commerce, scholarship, and access to a canon composed almost entirely in Hebrew and Aramaic.
% FOUNDING_PROBLEM_CORROBORATION: The Cairo Genizah's commercial letters and legal documents, the cross-border responsa collections, and surviving merchant formularies attest the founding problem and its solution from the record of use rather than from beneficiary self-description; by the interval's end, maskilic testimony and the visible migration of correspondence into European languages attest, from outside the benefiting parties, that the problem was increasingly being solved by other means.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement's costs are heavy and concentrated — roughly a decade of male childhood, household tuition, and the denial of literacy to half the population — while its dividends (trade facilitation, halakhic unity, canonical access) are real but unevenly distributed, so ε sits well above pure coordination cost yet short of predatory levels. Suppression 0.40 at interval end reflects an enforcement apparatus past its peak: communal bans on secular print and herem proclamations against maskilic circles mark the maximum (~t52), after which kehillah authority fragmented and enforcement decayed rapidly. Theater 0.42: early in the interval nearly all Hebrew production was load-bearing (ledgers, letters, responsa); as commerce migrated to vernaculars and state languages, a growing share of Hebrew output became ornamental, homiletic, or antiquarian — classic Goodhart drift of symbol over function. Accessibility_collapse 0.38: exits never closed — Aramaic, vernacular writing, and eventually European languages remained available and were increasingly exercised — which is why the arrangement needed enforcement at all. Resistance 0.48: individual grumbling was constant, organized resistance arrived with the Haskalah and vernacular print, and by interval end parents were withdrawing sons and correspondents were switching languages. The three series run on one shared seven-point grid (every tracked metric authored at every examined time point) so no end-state value is silently substituted into earlier rows. Gain_flow names rabbinic_leadership because the extracted inputs (tuition, childhood labor, enforced exclusivity) convert into scarcity of certified mastery, and that scarcity converts into adjudication income, communal office, and marriage-market position accruing to the administering seat; merchant gains are the coordination dividend open to any participant, not capture of the extracted input. Fixing_cost is prohibitive: any internal actor abandoning the medium forfeited legal unity, cross-family correspondence, and the canon simultaneously, which is why even the arrangement's fiercest critics proposed reform rather than abolition.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently, and the structural data explains why. From the rabbinic seat the arrangement is indispensable infrastructure that the seat itself administers; identity lock makes the administrator unable to imagine the medium's replacement, so the seat experiences the arrangement as the natural shape of Jewish life. From the student seat it is a decade of compulsory labor yielding competence in registers the child did not choose. From the women's seat it is a boundary-drawing exercise: the same households that funded sons' Hebrew literacy bought daughters' vernacular prayer-books, so the exclusion is experienced as the arrangement's most concrete fact. The excluded printer seat experiences the enforcement directly — bans, herems, relocation — while the merchant seat, holding arbitrage-grade exit, experiences the arrangement as a service it pays for implicitly and can leave. Coalition capacity deserves note: students are powerless singly and collectively (children cannot organize), and the women's potential numerical coalition was neutralized by the very literacy exclusion under examination — the mechanism suppresses the coalition that could contest it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Rabbinic_leadership sits nearest the beneficiary pole (d near 0.05–0.1): it declares the rules, collects the positional income, and its identity lock amplifies rather than dampens its subsidy, since exit would cost it its self-definition. Trans_diaspora_merchant_networks sit beneficiary-side but materially higher (d roughly 0.2): they consume the bridge function yet hold mobile exit, and agents with arbitrage-grade exit sit nearer the symmetric end because the arrangement cannot push costs onto them indefinitely — witness their actual migration to Ladino, Yiddish, and European languages. Heder_students sit near the full-target end (d roughly 0.85–0.9): trapped by age and dependency, they bear the transfer with no offsetting claim on its dividends. Diaspora_women likewise sit near the target end (d roughly 0.85) with the added feature that their exclusion removes even the partial beneficiary position the students' literacy eventually conferred. Vernacular_printers_authors stand outside the arrangement's benefit/cost flow as the suppressed alternative — their structural relationship registers as resistance pressure rather than seated directionality. Larger spatial scope (continental-to-global correspondence networks) raises verification difficulty and hence scales effective extraction upward for the trapped target seats; suppression itself remains unscaled, a raw property of the ban-and-herem machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — one shared medium for communities with mutually unintelligible vernaculars — was genuinely live for most of the interval and genuinely attenuating by its end: European state languages, postal networks, and print capitalism were solving cross-community communication by other means, and the coming revival would create a native-speaker condition this reading never contemplated. Hence founding_problem_status 'contested' paired with disappearance_verdict 'world_rearranges': the parties dispute whether the problem is dead, but everyone agrees arrangements currently depend on the medium. The theater trajectory (0.08 to 0.42) documents the functional hollowing that mandatrophy tracks — the arrangement increasingly maintained symbols of unity after the unity-work migrated elsewhere. Classification discipline prevents two opposite mislabelings. Reading the arrangement as pure rope erases the extraction: the child-labor cost, the tuition burden, and the gendered literacy allocation are not coordination overhead but positional transfers to the administering seat. Reading it as pure snare erases eight centuries of genuine coordination dividend — the Genizah trade networks and the halakhic unity they carried were real goods unavailable by any cheaper known mechanism. Tangled_rope keeps both faces legible and lets the engine's per-seat computation show who lived which face: the identity-locked administrator experiences coordination, the trapped child experiences extraction, and both experiences are data about the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the hebrew_continuity kernel correctly identifies where the language lives — instrumental bridge occupancy (this reading), preserved ritual recitation and textual transmission (liturgical_preservation), or native generative intuition (native_generative)?',
    'Trace the transmission chains against the 1880–1922 revival: determine which pre-revival substrate (bridge usage, liturgical recitation, or neither) could have regenerated a speech community, using the revival''s documented reliance on each as the discriminating evidence.',
    'If bridge occupancy is the operative substrate, this reading carries the kernel and the siblings'' ''not really Hebrew'' dismissal fails; if recitation alone suffices, this reading''s epsilon profile describes an epiphenomenon; if only nativization counts, all pre-revival readings fail together and the kernel''s pre-modern instantiation is void.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Contested instantiation of the hebrew_continuity kernel across three sibling readings.').

omega_variable(
    spontaneous_equilibrium_vs_enforced_monopoly,
    'Was the bridge-language arrangement a spontaneous network-effect equilibrium — a trade lingua franca emerging from utility — or an institutionally constructed and enforced monopoly over communal communication?',
    'Comparative history of enforcement lapses: examine communities under weak kehillah control and port-city populations with heavy outside contact; if Hebrew correspondence persisted where enforcement was absent, the arrangement is equilibrium; if it collapsed there, the enforcement machinery was load-bearing.',
    'An equilibrium finding pushes the classification toward the rope side with low suppression; a confirmed enforced-monopoly finding validates the tangled_rope claim and makes the enforcement apparatus, not participant preference, the thing to model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spontaneous_equilibrium_vs_enforced_monopoly, empirical, 'Whether the arrangement''s persistence reflects utility or enforcement.').

omega_variable(
    coordination_cost_vs_rent_attribution,
    'How much of the measured extraction is inherent coordination cost — any shared learned medium requires years of schooling — versus positional rent: register exclusivity, vernacular denigration, and gendered allocation of literacy?',
    'Compare cost profiles against functionally similar multilingual trade regimes (the Mediterranean Lingua Franca, Hanseatic Low German correspondence, Saharan commercial Arabic), controlling for baseline schooling burden, and isolate the components of cost unique to this arrangement''s exclusivity rules.',
    'If most extraction is inherent cost, effective chi drops toward the rope range; if rent dominates, the snare component strengthens and the victim seats compute harsher per-seat types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_vs_rent_attribution, conceptual, 'Attribution of measured extraction between coordination cost and positional rent.').

omega_variable(
    gender_exclusion_intrinsicness,
    'Was women''s exclusion from Hebrew literacy intrinsic to the bridge-language arrangement, or incidental to period-wide gender norms that would have excluded them from any formal literacy?',
    'Counterfactual comparison with settings where female literacy in sacred languages occurred — exceptional learned women, Karaite communities, later Alliance Israelite Universelle schools — testing whether this arrangement''s institutions actively maintained the exclusion (curriculum design, communal ordinance) or merely inherited ambient norms.',
    'If incidental, a share of epsilon and of the victim-seat severity attributed to this constraint belongs instead to the ambient gender order; if the arrangement''s institutions actively maintained exclusion, it is constraint-specific extraction and the victim seat computes accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_exclusion_intrinsicness, conceptual, 'Whether the victim structure''s gendered half is intrinsic to this constraint.').

omega_variable(
    register_boundary_epsilon_invariance,
    'Where does ''Hebrew'' end and the Judeo-vernacular continuum begin in marketplace and correspondence usage — and does drawing that boundary differently change the constraint''s epsilon?',
    'Corpus-linguistic stratification of Genizah documents and responsa by Hebrew-content ratio, then test whether the classification is stable across alternative boundary placements between pure-register usage and the Hebraized continuum.',
    'Per epsilon-invariance, if epsilon shifts with the observable, the colloquial label covers two structurally distinct constraints (pure-register usage versus the Hebraized contact continuum) and must decompose into separately authored stories linked by network edges; stability across boundaries certifies single-constraint status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(register_boundary_epsilon_invariance, conceptual, 'Boundary-drawing risk inside the ''Hebrew'' label; decomposition trigger check.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_bridge_pidgin_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(heb_bridge_pidgin_tr_t0, observed).
narrative_ontology:measurement(heb_bridge_pidgin_tr_t13, hebrew_continuity__bridge_pidginized, theater_ratio, 13, 0.1).
narrative_ontology:measurement_basis(heb_bridge_pidgin_tr_t13, observed).
narrative_ontology:measurement(heb_bridge_pidgin_tr_t26, hebrew_continuity__bridge_pidginized, theater_ratio, 26, 0.14).
narrative_ontology:measurement_basis(heb_bridge_pidgin_tr_t26, observed).
narrative_ontology:measurement(heb_bridge_pidgin_tr_t39, hebrew_continuity__bridge_pidginized, theater_ratio, 39, 0.2).
narrative_ontology:measurement_basis(heb_bridge_pidgin_tr_t39, observed).
narrative_ontology:measurement(heb_bridge_pidgin_tr_t52, hebrew_continuity__bridge_pidginized, theater_ratio, 52, 0.28).
narrative_ontology:measurement_basis(heb_bridge_pidgin_tr_t52, observed).
narrative_ontology:measurement(heb_bridge_pidgin_tr_t65, hebrew_continuity__bridge_pidginized, theater_ratio, 65, 0.36).
narrative_ontology:measurement_basis(heb_bridge_pidgin_tr_t65, observed).
narrative_ontology:measurement(heb_bridge_pidgin_tr_t78, hebrew_continuity__bridge_pidginized, theater_ratio, 78, 0.42).
narrative_ontology:measurement_basis(heb_bridge_pidgin_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(heb_bridge_pidgin_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(heb_bridge_pidgin_be_t0, observed).
narrative_ontology:measurement(heb_bridge_pidgin_be_t13, hebrew_continuity__bridge_pidginized, base_extractiveness, 13, 0.54).
narrative_ontology:measurement_basis(heb_bridge_pidgin_be_t13, observed).
narrative_ontology:measurement(heb_bridge_pidgin_be_t26, hebrew_continuity__bridge_pidginized, base_extractiveness, 26, 0.6).
narrative_ontology:measurement_basis(heb_bridge_pidgin_be_t26, observed).
narrative_ontology:measurement(heb_bridge_pidgin_be_t39, hebrew_continuity__bridge_pidginized, base_extractiveness, 39, 0.64).
narrative_ontology:measurement_basis(heb_bridge_pidgin_be_t39, observed).
narrative_ontology:measurement(heb_bridge_pidgin_be_t52, hebrew_continuity__bridge_pidginized, base_extractiveness, 52, 0.66).
narrative_ontology:measurement_basis(heb_bridge_pidgin_be_t52, observed).
narrative_ontology:measurement(heb_bridge_pidgin_be_t65, hebrew_continuity__bridge_pidginized, base_extractiveness, 65, 0.63).
narrative_ontology:measurement_basis(heb_bridge_pidgin_be_t65, observed).
narrative_ontology:measurement(heb_bridge_pidgin_be_t78, hebrew_continuity__bridge_pidginized, base_extractiveness, 78, 0.58).
narrative_ontology:measurement_basis(heb_bridge_pidgin_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(heb_bridge_pidgin_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(heb_bridge_pidgin_su_t0, observed).
narrative_ontology:measurement(heb_bridge_pidgin_su_t13, hebrew_continuity__bridge_pidginized, suppression_requirement, 13, 0.49).
narrative_ontology:measurement_basis(heb_bridge_pidgin_su_t13, observed).
narrative_ontology:measurement(heb_bridge_pidgin_su_t26, hebrew_continuity__bridge_pidginized, suppression_requirement, 26, 0.53).
narrative_ontology:measurement_basis(heb_bridge_pidgin_su_t26, observed).
narrative_ontology:measurement(heb_bridge_pidgin_su_t39, hebrew_continuity__bridge_pidginized, suppression_requirement, 39, 0.58).
narrative_ontology:measurement_basis(heb_bridge_pidgin_su_t39, observed).
narrative_ontology:measurement(heb_bridge_pidgin_su_t52, hebrew_continuity__bridge_pidginized, suppression_requirement, 52, 0.62).
narrative_ontology:measurement_basis(heb_bridge_pidgin_su_t52, observed).
narrative_ontology:measurement(heb_bridge_pidgin_su_t65, hebrew_continuity__bridge_pidginized, suppression_requirement, 65, 0.55).
narrative_ontology:measurement_basis(heb_bridge_pidgin_su_t65, observed).
narrative_ontology:measurement(heb_bridge_pidgin_su_t78, hebrew_continuity__bridge_pidginized, suppression_requirement, 78, 0.4).
narrative_ontology:measurement_basis(heb_bridge_pidgin_su_t78, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, information_standard).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial notion 'Hebrew stayed alive' decomposes into three structurally distinct constraints sharing one kernel (hebrew_continuity). This member (bridge_pidginized) authors the instrumental contact-language regime with its own epsilon (0.58), its own beneficiary/victim structure (merchant and rabbinic beneficiaries; students and women as victims), and its own failure mode (functional hollowing as commerce migrates to vernaculars). The upstream members differ: liturgical_preservation covers the recitation-and-transmission channel with negligible extraction over its core referent, while native_generative covers the nativization criterion that only became satisfiable with the modern revival. This reading sits downstream of liturgical_preservation (recitation supplied the phonological and textual substrate the bridge register drew on) and stands in logical tension with native_generative, whose exclusivity premise this reading's foundational axiom denies. Each file links the others via affects_constraints; no single file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
