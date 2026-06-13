% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Medina as Canonical Practice Source
 *   domain: legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   The Maliki reading of Islamic jurisprudential method claims that law
 *   derives from Qur'an and Hadith as mediated through the living practice of
 *   the Medinan scholarly community ('amal ahl al-Madina). This reading
 *   resolves the tension between revelation (which ceased with the Prophet)
 *   and jurisprudential consistency (which requires application to novel
 *   cases) by appealing to Medina's unique status as the Prophet's city and
 *   the seat of the earliest Muslim community. The Maliki school
 *   institutionalized this reading across North Africa, parts of the Levant,
 *   and Egypt, establishing Medinan scholarly consensus as a legitimate third
 *   source of law. This constraint represents the structural effect of that
 *   claim: it creates a hierarchy where Medinan-custodian scholars hold
 *   interpretive authority, where non-Maliki schools must either accept
 *   secondary status or continuously defend alternative methods, and where
 *   later-generation scholars must prove innovation through fidelity to 'amal
 *   rather than pure reasoning. The CLAIMED type is tangled_rope because the
 *   constraint genuinely coordinates jurisprudential consistency (the
 *   coordination problem) while asymmetrically privileging Medinan scholars
 *   (the extraction function). The authored metrics describe medium-range
 *   extraction with moderate suppression—the method's internal coherence
 *   makes non-Maliki schools' exclusion structurally elegant but not brutal.
 *   The measurement series tracks stabilization: extractiveness rises in the
 *   first two centuries (as the Maliki method institutionalizes and regional
 *   schools are positioned as challengers) and plateaus by century 5 (the
 *   institutional order solidifies). Theater ratio rises gently throughout,
 *   reflecting increasing emphasis on maintaining 'amal fidelity as actual
 *   practice diverges from early-era norms.
 *
 * KEY AGENTS:
 *   - Medinan scholarly lineage: Custodian of 'amal, sets agenda for what counts as valid practice, holds interpretive authority
 *   - Non-Medinan interpretive claimants: Hanafi, Hanbali, Shafi'i schools developing competing methods, structurally disadvantaged by the Maliki reading's appeal to geographical authenticity
 *   - Legal practitioners and judges: Benefit from method coherence and institutional stability, constrained by requirement to validate against 'amal
 *   - Mujtahid jurists: Powerful scholars with capacity for independent reasoning, some benefit from Maliki institutional support, non-Maliki face continuous legitimacy challenges
 *   - Competing juridical schools: Excluded from equal standing in Maliki-dominant regions, can develop methods but lack geographical/genealogical authority
 *   - Later-generation scholars: Inherit constraint, must prove innovation through practice-fidelity rather than pure reasoning
 *   - Analytical observer: Examines structural effects and unfalsifiability of 'amal claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.48).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.52).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: Medina as Canonical Practice Source").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, 'b1e075c7-7093-47f2-92d2-649eb2ac16f9').
narrative_ontology:cs_kernel_codification('b1e075c7-7093-47f2-92d2-649eb2ac16f9', fixed_text).
narrative_ontology:cs_authority_grounding('b1e075c7-7093-47f2-92d2-649eb2ac16f9', lineage).
narrative_ontology:cs_interpretation_layer_present('b1e075c7-7093-47f2-92d2-649eb2ac16f9').
narrative_ontology:cs_reading_relation('b1e075c7-7093-47f2-92d2-649eb2ac16f9', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1e075c7-7093-47f2-92d2-649eb2ac16f9', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1e075c7-7093-47f2-92d2-649eb2ac16f9', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('b1e075c7-7093-47f2-92d2-649eb2ac16f9', foundational, medinan_practice_authenticity).
narrative_ontology:cs_axiom_status(medinan_practice_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('b1e075c7-7093-47f2-92d2-649eb2ac16f9', medinan_practice_authenticity, conventional).
narrative_ontology:cs_axiom('b1e075c7-7093-47f2-92d2-649eb2ac16f9', foundational, geographical_proximity_legitimacy).
narrative_ontology:cs_axiom_status(geographical_proximity_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b1e075c7-7093-47f2-92d2-649eb2ac16f9', geographical_proximity_legitimacy, deontological).
narrative_ontology:cs_reference_frame('b1e075c7-7093-47f2-92d2-649eb2ac16f9', prophetic_practice_medina_preservation).
narrative_ontology:cs_drift_state('b1e075c7-7093-47f2-92d2-649eb2ac16f9', contemporary_historiographical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b1e075c7-7093-47f2-92d2-649eb2ac16f9', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, legal_practitioners_judges).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, mujtahid_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, legal_practitioners_judges).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, later_generation_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preserves and transmits the Maliki method across generations, claiming direct custodianship of Prophetic practice through unbroken Medinan tradition. Administers what counts as valid 'amal (living practice) and judges competing claims to authenticity. Their interpretive authority rests on geographical and genealogical proximity to the Prophet's practice and the scholarly lineage's continuity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Scholars from Iraq, Egypt, Khurasan, and other regions who develop equally rigorous jurisprudential methods but lack the Maliki reading's appeal to unbroken Medinan practice. They pay the cost of having their claims to equal authenticity structurally disadvantaged by the framework that privileges geographic/genealogical proximity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claimants, payer,
    organized, civilizational, constrained, continental).

% Use Maliki jurisprudence in adjudication and fatwa-giving. Benefit from the method's coherence and institutional stability. Constrained by the method's requirement to validate judgments against 'amal, which limits interpretive flexibility.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, legal_practitioners_judges, beneficiary,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, legal_practitioners_judges, payer).

% Independent jurists with strong enough scholarly reputation to develop and defend their own methods. Maliki scholars among them benefit from the method's institutional legitimacy. Non-Maliki mujtahids face institutional pressure and must defend their methods continuously.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, mujtahid_jurists, beneficiary,
    powerful, civilizational, arbitrage, continental).

% Hanafi, Hanbali, and Shafi'i schools developed equally rigorous methods but are structurally positioned as challengers to Maliki authority in regions where the Maliki reading institutionalized. Their exclusion is from equal standing in Maliki-dominant regions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, competing_juridical_schools, excluded,
    institutional, civilizational, trapped, continental).

% Scholars in centuries after the first three generations who inherit the constraint and must decide whether to follow established Maliki practice or attempt methodological innovation. They bear the cost of having new interpretations scrutinized through the 'amal lens.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, later_generation_scholars, payer,
    organized, generational, constrained, continental).

% Examines the Maliki reading's structural role in Islamic jurisprudence, observing how the claim to Medinan practice authenticity enables institutional dominance and how it constrains non-Maliki schools.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine problem in early Islamic jurisprudence: how to extend the Prophet's guidance (Qur'an and Hadith) to novel situations and ensure legal consistency across geographically dispersed Muslim communities. The Maliki reading proposes that Medina preserved the Prophet's practice most faithfully, so actual Medinan scholarly consensus and long-standing practice ('amal) serve as a third source of law alongside revelation and hadith transmission. This provides a stable, observable reference point for resolving novel cases.
% TRANSFER_FUNCTION: Moves interpretive authority from competing regional scholarly traditions (Iraq, Egypt, Khurasan) toward Medinan-custodian scholars. The transfer flows as: (1) recognition of Medinan practice as a valid jurisprudential source (structural advantage in legitimacy claims), (2) deferential treatment of Medinan scholarly consensus in law-finding, and (3) institutional dominance of Maliki jurisprudence in regions that adopted it as the official school. Non-Maliki schools lose claims to equal authenticity—their methods are valid but lack the Maliki reading's appeal to geographical/genealogical proximity to the Prophet.
% ABSENT_VOICES: Non-Medinan jurists who might claim equal fidelity to Prophetic practice through different methods (Hanafi reliance on qiyas, Hanbali literalism, Shafi'i hadith-centered hierarchy). The Maliki framework structures them out: their absence is maintained by the requirement that novel interpretations prove fidelity to Medinan practice—a criterion non-Medinan schools can neither fully satisfy nor decisively refute. Later-generation scholars who attempted methodological innovation beyond established 'amal would also object if they had structural voice; their objections are absorbed into the constraint as the need to show continuity with practice.
% DISAPPEARANCE_RATIONALE: If the Maliki reading's claim to Medinan practice authority vanished, Islamic jurisprudence would revert to a multi-center system where Hanafi, Hanbali, Shafi'i, and other methods compete on methodological grounds alone, without the Maliki framework's appeal to geographical authenticity. Regions that institutionalized Maliki law would face pressure to either reformulate legitimacy on text-based grounds or acknowledge methodological pluralism. The constraint's disappearance would not dissolve jurisprudence itself but would eliminate a primary source of institutional hierarchy among the schools.
% FOUNDING_PROBLEM: How to generate consistent legal guidance for an expanding Muslim community, centuries after the Prophet, when direct revelation has ceased and hadith transmission is increasingly complex and sometimes disputed. How to ensure that local innovations in practice do not diverge from the Prophet's original intent. Medina, as the Prophet's city and the seat of the earliest Muslim community, represents a living laboratory where the Prophet's practice was most visibly instantiated—its preserved customs offer a window into what the Prophet actually approved and how his Companions applied his guidance.
% FOUNDING_PROBLEM_CORROBORATION: The Maliki scholarly lineage attests the problem is still live and that Medinan practice remains the most reliable source. Non-Maliki schools (represented in later jurisprudential discourse) contest the solution: they argue the founding problem is better solved through systematic hadith criticism (Shafi'i), literal Qur'anic reading (Hanbali), or careful reasoning within textual constraints (Hanafi). Modern Islamic historians debate whether Medinan practices documented in later sources accurately reflect the Prophet's era or are reconstructions of a golden age (Ibn Warraq, Hallaq, Tyan). Independent corroboration that Medina uniquely preserved Prophetic practice comes primarily from later Maliki scholarship itself; competing schools developed precisely because other regions developed different reconstructions of what authentic jurisprudential method should be.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.48 reflects medium-range asymmetry: the constraint genuinely solves coordination (jurisprudential consistency) but does so by elevating Medinan scholars' interpretive claims above all others. Non-Maliki schools are not excluded from jurisprudence entirely but face a permanent legitimacy deficit—their methods must be defended as alternatives to a reading grounded in geographical proximity, which is difficult to refute empirically. Suppression at 0.52 indicates moderate active maintenance: the constraint persists partly through institutional dominance (Maliki courts, Maliki-trained judges in North Africa/Egypt) and partly through the internal coherence of the method itself (appeals to 'amal are elegant and difficult to dismiss without developing competing appeal-to-practice arguments). Theater ratio at 0.38 suggests the method's functional core remains substantial: 'amal examination and practice-consistency checking are real activities, not pure performance, but increasingly the ritual of consulting 'amal serves to validate decisions already reached through other means (institutional preference, local necessity). Accessibility collapse at 0.62: once a scholar accepts that Medinan practice is a valid jurisprudential source, alternatives (pure reasoning, text-only methods, competing geographical authorities) seem less appealing—the framework is internally coherent. But the collapse is not total (0.85+) because competing schools have developed equally coherent frameworks, making the choice between methods partly a matter of tradition and institutional location. Resistance at 0.58: the Maliki reading meets substantial resistance from schools committed to different methods (Hanafi analogical reasoning, Hanbali literalism, Shafi'i hadith hierarchy), but resistance is structurally organized as methodological disagreement rather than outright rejection of jurisprudence itself.
 *
 * PERSPECTIVAL GAP:
 *   The Medinan scholarly lineage seat would experience the constraint as genuine coordination it stewarded into coherence—they preserved practice, systematized it, defended its validity, and created institutional structures that work. From their seat, the constraint is almost rope-like (real coordination, modest enforcement overhead). Non-Medinan scholars would experience the same structure as systematic disadvantage: they developed equally rigorous methods but cannot claim the legitimacy of geographical/genealogical proximity to the Prophet, so their interpretations require continuous defense and never fully escape secondary status. From their seat, the constraint resembles snare-like extraction—their exclusion from equal standing is maintained by active institutional pressure (Maliki-dominated courts, Maliki-trained judges, preference for Maliki jurists in fatwas) and by the framework's unfalsifiable appeal to 'amal (how can one prove Medina did NOT preserve practice most faithfully?). Legal practitioners sit near symmetric: they benefit from the method's coherence and institutional stability, but also bear the cost of being bound to 'amal-validation, which constrains their flexibility. The engine computes per-seat type from directionality and power; this gap between Medinan-setter and non-Maliki-payer seats should produce divergent classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Medinan scholarly lineage: power=institutional, exit=identity_locked (leaving Medinan custodianship means leaving Maliki identity), beneficiary of the constraint's institutional dominance → directionality near 0.0 (full beneficiary, subsidy). Non-Medinan interpretive claimants: power=organized, exit=constrained (cannot fully exit competing schools without abandoning jurisprudential tradition, yet face permanent secondary status), victims of the framework's structural disadvantage of non-geographical claims → directionality near 0.8 (high target, constrained/trapped exit). Legal practitioners/judges: power=powerful, exit=mobile (can practice in different schools' jurisdictions), beneficiary of method's stability but payer of fidelity-to-practice constraint → directionality near 0.5 (symmetric). Mujtahid jurists: power=powerful, exit=arbitrage (can develop competing methods or shift traditions), mixed position → directionality depends on whether Maliki-affiliated or non-Maliki (Maliki mujtahid d≈0.2, non-Maliki mujtahid d≈0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mislabeling pure extraction as coordination through the stakeholder surface: the coordination function (jurisprudential consistency for geographically dispersed communities) is real and genuine, but the extraction function (subordination of non-Maliki schools) is also structural. Tangled_rope correctly captures this hybrid: not pure coordination (rope) because non-Maliki schools pay a permanent legitimacy cost; not pure extraction (snare) because the method genuinely solves the founding coordination problem. The constraint's mandate—to ground jurisprudence in Prophetic practice while enabling legal consistency—is live and contested: Maliki schools maintain it is still necessary (Medina still represents the best-preserved practice), while modern Islamic historians argue the founding problem shifted centuries ago (now jurisprudential consistency requires something other than Medinan practice). This shifting relationship between founding problem and current solution is captured in six_questions.founding_problem_status = contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amal_historical_reconstruction,
    'Were the Medinan practices documented by later Maliki scholars actually operative in the Prophet''s era and the first generations, or are they reconstructions/idealized retrospections created in later centuries?',
    'Comparative textual analysis of early hadith collections, Maliki jurisprudential treatises, and non-Maliki sources to identify anachronisms, later interpolations, and conflicts between claimed practices. Archaeological or documentary evidence about Medinan judicial procedure (if available). Genealogical analysis of the isnads (transmission chains) claiming direct knowledge of ''amal.',
    'If actual practices were documented contemporaneously and transmitted faithfully, ''amal is an empirically grounded source and the Maliki reading''s appeal to geographical authenticity is well-founded. If Medinan practices are largely reconstructions, the Maliki reading''s legitimacy rests on interpretive narrative rather than verifiable practice—this would elevate the constraint''s suppression (must actively maintain the narrative) and reframe it as possible false-summit mountain (claiming natural practice while actually constructing it). ε would shift from medium (genuine practice-based coordination) to higher extraction (tradition legitimizing interpretive dominance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_historical_reconstruction, empirical, 'The historical status of Medinan practice as reconstructed source vs. lived reality').

omega_variable(
    methodological_pluralism_counterfactual,
    'Would Islamic jurisprudence have developed more coherently if all schools were permitted equal standing without geographical/genealogical hierarchy, or does the Maliki reading''s appeal to Medinan practice provide necessary institutional stability?',
    'Historical comparison with non-Maliki-dominant regions (where Hanafi, Hanbali, Shafi''i schools operated as primary authorities) to examine whether those jurisdictions experienced greater legal inconsistency or methodological chaos. Analysis of how Maliki courts actual handled disputes where ''amal was ambiguous or conflicting. Counterfactual reconstruction: if no school claimed special authenticity, what institutional mechanism would have coordinated jurisprudential consistency?',
    'If non-Maliki regions achieved comparable coherence without geographical authority, the Maliki reading''s extraction function is exposed as unnecessary hierarchy maintained for institutional dominance—ε and suppression would rise, moving the constraint closer to snare. If Maliki regions achieved superior consistency precisely because of ''amal coordination, the reading is descriptively justified for its coordination function, and any extraction is genuine coordination cost, not pure rent-seeking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(methodological_pluralism_counterfactual, conceptual, 'Whether geographical/genealogical authority was structurally necessary for jurisprudential stability or represents unnecessary institutional hierarchy').

omega_variable(
    unfalsifiability_of_practice_claims,
    'Can the claim that Medina preserved Prophetic practice most faithfully ever be empirically falsified, or is the appeal to ''amal inherently unfalsifiable and therefore non-science?',
    'Epistemological analysis of what would count as refutation: could historians find evidence that non-Medinan practices were closer to the Prophet''s teaching? Could direct sources (contemporary documents, explicit Companion statements) demonstrate that Medinan practice diverged from the Prophet''s intent? If no possible evidence would refute the Maliki reading, it is non-falsifiable and functions as an interpretive frame rather than an empirical claim.',
    'If the claim is structurally unfalsifiable, it is a commitment system commitment (CS_structure application: the Maliki reading is a fixed interpretive frame, not an empirical discovery). This would mean the constraint operates partly through suppression of competing frames rather than through superior explanatory power. ε might remain stable (0.48) but the mechanism would be reframed from legitimate coordination to institutional authority maintenance. Theater_ratio might rise (the claim to ''amal is performatively maintained because it cannot be falsified, only defended). This omega feeds into whether the constraint is best modeled as tangled_rope (genuine coordination + extraction) or as extraction-dominated with a coordination cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unfalsifiability_of_practice_claims, conceptual, 'The epistemological status of ''amal claims: empirical, interpretive, or non-falsifiable commitment').

omega_variable(
    identity_lock_vs_genuine_choice,
    'Do Maliki scholars remain within the tradition because the ''amal framework is genuinely superior (rational choice), or because Medinan custodianship is fused with Maliki identity such that exit would constitute existential loss (identity lock)?',
    'Sociological/biographical analysis of Maliki scholars who attempted methodological innovation or considered adopting competing schools. Analysis of conversion paths: do scholars switch between schools and what accounts for switches? Textual analysis of how Maliki jurisprudence frames Medinan practice as essence vs. contingent feature. Comparative analysis with other identity-locked traditions (monastic orders, professional guilds).',
    'If scholars exit is primarily identity-locked (relationship to tradition as core self-definition), the constraint''s suppression is internalized rather than structural. This affects how classification diverges per-seat: Maliki custodians might compute as snare-bound (trapped by identity, despite nominal choice) despite institutional power. Non-Maliki scholars also experience identity lock (cannot fully exit their schools) combined with structural disadvantage, making their situation more snare-like. If scholars rationally choose Maliki because they find ''amal-grounding superior, the exit is less identity-locked and the constraint is more consensual coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_genuine_choice, empirical, 'Whether Maliki identity lock is structural (identity fusion) or rational (superior methodology)').

omega_variable(
    sibling_reading_coexistence,
    'Are the four jurisprudential readings (Maliki, Hanafi, Hanbali, Shafi''i) genuinely coexistent as equally valid approaches to the same kernel, or does one reading''s dominance structurally foreclose the others'' legitimacy within shared institutional spaces?',
    'Historical analysis of how multiple schools functioned in regions where they coexisted (medieval Cairo, Baghdad, Ottoman empire). Examination of whether Maliki dominance in certain regions (North Africa) prevented equal standing for other schools, or whether schools achieved peaceful coexistence. Analysis of contemporary Islamic jurisprudence: do the four schools coexist as valid options or does one claim dominance?',
    'If readings genuinely coexist without foreclosure, the cs_structure.reading_relations should be coexists_with for all sibling pairs. If Maliki dominance structurally forecloses or substantially influences sibling schools in overlapping jurisdictions, relations should be forecloses or influences. This affects how the constraint''s extraction function is understood: coexistence suggests pluralistic competition (lower suppression); dominance suggests hierarchical extraction (higher suppression). ε might remain 0.48, but the suppression value (currently 0.52) might shift to 0.65+ if dominance is established.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'The actual coexistence of sibling readings: peaceful pluralism vs. hierarchical dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(juri_tr_t0, projected).
narrative_ontology:measurement(juri_tr_t50, jurisprudential_method_kernel__maliki_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(juri_tr_t50, observed).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__maliki_reading, theater_ratio, 100, 0.32).
narrative_ontology:measurement_basis(juri_tr_t100, observed).
narrative_ontology:measurement(juri_tr_t150, jurisprudential_method_kernel__maliki_reading, theater_ratio, 150, 0.36).
narrative_ontology:measurement_basis(juri_tr_t150, observed).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 200, 0.37).
narrative_ontology:measurement_basis(juri_tr_t200, observed).
narrative_ontology:measurement(juri_tr_t250, jurisprudential_method_kernel__maliki_reading, theater_ratio, 250, 0.38).
narrative_ontology:measurement_basis(juri_tr_t250, observed).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__maliki_reading, theater_ratio, 300, 0.38).
narrative_ontology:measurement_basis(juri_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(juri_be_t0, projected).
narrative_ontology:measurement(juri_be_t50, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(juri_be_t50, observed).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 100, 0.46).
narrative_ontology:measurement_basis(juri_be_t100, observed).
narrative_ontology:measurement(juri_be_t150, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement_basis(juri_be_t150, observed).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 200, 0.49).
narrative_ontology:measurement_basis(juri_be_t200, observed).
narrative_ontology:measurement(juri_be_t250, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 250, 0.48).
narrative_ontology:measurement_basis(juri_be_t250, observed).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 300, 0.48).
narrative_ontology:measurement_basis(juri_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(juri_su_t0, projected).
narrative_ontology:measurement(juri_su_t50, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(juri_su_t50, observed).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement_basis(juri_su_t100, observed).
narrative_ontology:measurement(juri_su_t150, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 150, 0.51).
narrative_ontology:measurement_basis(juri_su_t150, observed).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 200, 0.52).
narrative_ontology:measurement_basis(juri_su_t200, observed).
narrative_ontology:measurement(juri_su_t250, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 250, 0.52).
narrative_ontology:measurement_basis(juri_su_t250, observed).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 300, 0.52).
narrative_ontology:measurement_basis(juri_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__maliki_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel is a contested institutional commitment whose interpretation splits into four structurally distinct constraints corresponding to the four major Islamic jurisprudential schools. This constraint instantiates the MALIKI READING, which claims law derives from Qur'an/Hadith as practiced in the Medinan community. The sibling readings (Hanafi, Hanbali, Shafi'i) each ground law in different sources and methods (reason-extended qiyas, literal-text literalism, hadith-transmission hierarchy). ε values differ substantially across readings: Maliki medium (0.48 on custom/practice authority), Hanafi higher on rationalist extension, Hanbali high on text-literalism constraint, Shafi'i medium on systematic classification. Beneficiary sets differ: Maliki benefits Medinan scholarly custodians; Hanafi benefits rationalist jurists; Hanbali benefits purist literalists; Shafi'i benefits systematic methodologists. Victim sets differ: Maliki's non-Medinan claimants; Hanafi's text-conservative objectors; Hanbali's reason-advocates; Shafi'i's traditionalist objectors. Each reading represents a distinct constraint and must be modeled separately with its own ε, beneficiary/victim structure, and stakeholders. They affect one another through cs_structure.reading_relations and institutional pressure (if one reading dominates in shared jurisdictions, others' scope for operation contracts), but they are NOT observational variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__maliki_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
