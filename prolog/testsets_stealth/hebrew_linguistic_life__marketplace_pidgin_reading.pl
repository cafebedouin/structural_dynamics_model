% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Death-and-Revival Periodization of Hebrew (Marketplace-Pidgin Reading)
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   The standing arrangement under contest is the death-and-revival
 *   periodization: the received account, institutionalized in school
 *   curricula, commemorative ritual, and the charter self-understanding of
 *   the Hebrew Language Academy, that Hebrew ceased to be spoken in antiquity
 *   and was recreated as a vernacular by the late nineteenth-century revival
 *   movement. This file instantiates the marketplace_pidgin_reading of the
 *   kernel hebrew_linguistic_life, under which a language is alive when it
 *   functions as an inter-communal medium of practical coordination
 *   regardless of native-speaker status or sacred function. By that reading's
 *   own lights, the standing regime misclassifies roughly seventeen centuries
 *   of documented market, epistolary, and judicial Hebrew as death, and
 *   transfers the credit for spoken Hebrew to the revival generation. The
 *   epsilon referent is the standing periodization regime as this reading
 *   assesses it, never any arrangement this reading would endorse. Sibling
 *   readings of the same kernel are separate constraint files, not parts of
 *   this one. The claim/metric gap is deliberate: the constraint is CLAIMED
 *   as tangled_rope while the metrics independently describe substantially
 *   extractive, actively enforced, partly theatrical operation; the engine
 *   measures the divergence.
 *
 * KEY AGENTS:
 *   - national_curriculum_authorities: agenda-setter (institutional/arbitrage) — administers the periodization through approved curricula and commemorative funding
 *   - hebrew_language_academy: primary beneficiary (institutional/identity_locked) — collects charter legitimacy and international standing from the revival narrative
 *   - sephardic_mizrahi_jerusalem_communities: primary target (organized/identity_locked) — their continuous spoken use is footnoted while the rebirth story takes the credit
 *   - palestinian_arab_market_partners: excluded participant (powerless/trapped) — the unrecorded half of the market pidgin exchange
 *   - continuity_scholars: analytical observer (analytical/analytical) — sees the full documentary structure, answers to peer review alone
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.6).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.5).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Death-and-Revival Periodization of Hebrew (Marketplace-Pidgin Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'fe43271b-5326-482c-9214-4f98390c3184').
narrative_ontology:cs_kernel_codification('fe43271b-5326-482c-9214-4f98390c3184', distributed).
narrative_ontology:cs_authority_grounding('fe43271b-5326-482c-9214-4f98390c3184', distributed).
narrative_ontology:cs_reading_relation('fe43271b-5326-482c-9214-4f98390c3184', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe43271b-5326-482c-9214-4f98390c3184', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_axiom('fe43271b-5326-482c-9214-4f98390c3184', foundational, practical_intercommunal_function_constitutes_life).
narrative_ontology:cs_axiom_status(practical_intercommunal_function_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('fe43271b-5326-482c-9214-4f98390c3184', practical_intercommunal_function_constitutes_life, empirically_contingent).
narrative_ontology:cs_axiom('fe43271b-5326-482c-9214-4f98390c3184', secondary, native_acquisition_not_required_for_vitality).
narrative_ontology:cs_axiom_status(native_acquisition_not_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('fe43271b-5326-482c-9214-4f98390c3184', native_acquisition_not_required_for_vitality, conventional).
narrative_ontology:cs_reference_frame('fe43271b-5326-482c-9214-4f98390c3184', intercommunal_practical_use_baseline).
narrative_ontology:cs_drift_state('fe43271b-5326-482c-9214-4f98390c3184', post_continuity_scholarship_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe43271b-5326-482c-9214-4f98390c3184', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, national_curriculum_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, sephardic_mizrahi_jerusalem_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, palestinian_arab_market_partners).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, language_death_periodization_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, revival_founder_attribution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Approve the textbooks and exam syllabi that teach Hebrew's history as a death around the third century followed by a late-nineteenth-century rebirth. They convene the committees deciding which scholarship enters the classroom, fund the commemorative programs tied to the founder's biography, and could revise the periodization by administrative decision, though every revision carries political cost with parents, clergy, and veteran teachers.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, national_curriculum_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% The statutory body setting Hebrew norms worldwide. Its founding charter and public identity are built on being the heir of the revival: museum partnerships, annual founder-day ceremonies, and international standing all presuppose the rebirth story. Abandoning that story would unsettle the institution's own reason for being as currently constituted, so it maintains the narrative even as its own historical staff document earlier spoken varieties.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy, beneficiary,
    institutional, generational, identity_locked, global).

% Kept Hebrew in daily spoken use across the centuries the standard history calls dead, in Jerusalem's markets, in letters between distant communities, and in court mediation, yet watch that continuity get footnoted as an anomaly while the rebirth story takes the credit. Their communal archives and oral-history projects carry the counter-record. Walking away from the claim would mean disowning their own grandparents' speech, so they press it despite thin institutional reward.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, sephardic_mizrahi_jerusalem_communities, payer,
    organized, generational, identity_locked, regional).

% Traded, haggled, and contracted with Hebrew-speaking counterparts in the Old City's markets, picking up and shaping the working variety in the process. They appear in the historiography, when at all, as background scenery. No archive collects their side of the conversation, no institution represents them in Hebrew-language debates, and the channels through which their testimony could enter the record barely exist.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, palestinian_arab_market_partners, excluded,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__marketplace_pidgin_reading, palestinian_arab_market_partners, payer).

% Philologists and sociolinguists compiling the traveler diaries, merchant letters, and court records showing Hebrew in continuous practical use. They take no side in the national argument and answer to peer review rather than to any ministry; their leverage is publication and citation, and their findings reach classrooms only after passing through the very committees whose framing they complicate.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, continuity_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__marketplace_pidgin_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives educators, scholars, and state institutions a single shared periodization of Hebrew's history: a common timeline and vocabulary for curricula, examinations, commemoration, and research, so teaching and heritage administration proceed without renegotiating first principles each time.
% TRANSFER_FUNCTION: Moves historical recognition and narrative prestige from the communities that continuously used Hebrew as a spoken inter-communal medium, Sephardi and Mizrahi Jerusalemites together with their Arab trading partners, to the late nineteenth-century revival movement and its institutional heirs, who are credited with producing spoken Hebrew from nothing.
% ABSENT_VOICES: The Arab merchants and neighbors who spoke the market variety left almost no written record and hold no seat in Hebrew-language historiography; within the Jewish communities, working-class women, who carried much of the daily market talk, are doubly missing from a record built on male textual production. Both would contest a history told without them.
% DISAPPEARANCE_RATIONALE: If the death-and-revival periodization vanished overnight, curricula would be rewritten, the commemorative calendar anchored to the founder's biography would lose its anchor, the Academy's charter narrative would shift, and the story of Hebrew's continuity would move from miraculous rebirth to unbroken adaptation, redistributing recognition across fifteen centuries of users.
% FOUNDING_PROBLEM: The Yishuv-era movement needed to demonstrate that a nation returning to its land could also resurrect its tongue: a dead language revived proved national creativity, rootedness, and the feasibility of the whole enterprise against skeptics who held spoken Hebrew to be impossible.
% FOUNDING_PROBLEM_CORROBORATION: International scholarship on language planning and invented traditions treats the death-and-revival frame as a mobilization artifact rather than a neutral description, and the continuity-documenting philologists outside the revival institutions corroborate that the periodization hardened for pedagogical and diplomatic reasons. Within the benefiting institutions the problem is attested as still live; no source outside them defends that liveness.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.60 at interval end because the regime appropriates historical recognition rather than material goods: substantial, but bounded by the fact that academic scholarship does acknowledge pre-revival spoken varieties at the margins. Suppression is 0.50 and is authored as a raw structural property, unscaled by power or scope: continuity scholarship publishes, but reaches classrooms only through committee gatekeeping. Theater is 0.50 because roughly half the regime's visible activity is commemorative performance (founder-day ritual, museum narrative, anniversary journalism) rather than descriptive linguistics. Accessibility collapse is low (0.30): once the criterion dispute is understood, the rival criteria remain fully available; nothing forecloses them. Resistance is 0.60: continuity philology, community archives, and rival-criterion holders actively contest the periodization. The temporal series run on one shared ten-point grid so every tracked metric is authored at every examined time point. The arc shows a canonization ratchet (rising extraction and enforcement through statehood-era curriculum consolidation, peaking around mid-century) followed by contestation-driven relaxation after the historiographic debates of the 1980s-1990s began eroding the monopoly; the oscillation is not cyclical reinforcement but a secular rise-and-partial-decline, so no intermittent-reinforcement mechanism is alleged. Coalition potential is real: joint Sephardi-Mizrahi and Palestinian shared-history projects could convert the two diffuse victim seats into an organized bloc, which is why their current power ratings (organized, powerless) should be read as contingent rather than fixed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the curriculum-authority seat the periodization is pedagogical order: a shared timeline that lets a school system function. From the Academy seat it is custodianship of a founding achievement. From the Sephardi-Mizrahi community seat the same structure operates as expropriation of their grandparents' daily speech. From the Arab market-partner seat it is a history told in their absence. From the analytical seat it is a measurable distortion between the documentary record and the transmitted narrative. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The curriculum authorities are agenda-setters who also benefit (low d, near the beneficiary end): they administer the arrangement and collect pedagogical coherence and legitimacy from it, with arbitrage-grade exit since they can revise curricula administratively. The Academy is a declared beneficiary with identity_locked exit: the lock deepens its commitment to maintaining the arrangement rather than raising its exposure to extraction, so it sits firmly at the beneficiary end. The Sephardi-Mizrahi communities are victims with identity_locked exit and organized capacity: high d, amplified by the lock, since they cannot abandon the claim without disowning their own past. The Arab market partners are victims who are trapped and powerless with no representational channel: nearest the full-target end, since they bear the erasure with zero exit and zero voice. The continuity scholars are analytical observers near symmetric: they neither collect the narrative rents nor bear the erasure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, mobilization-era legitimation of spoken Hebrew as proof of national feasibility, has partially outlived its original urgency: the state exists, the vernacular won, and the narrative now serves identity maintenance and pedagogical convenience more than the original fight. The tangled_rope claim keeps both truths visible: a snare label would erase the genuine coordination a shared periodization provides to teaching and research, while a rope label would launder the recognition transfer away from the continuity communities. The founding_problem_status is authored as contested rather than dead because the legitimation need persists in attenuated form inside the benefiting institutions; the mismatch consumer therefore finds no dead-mandate-plus-world_rearranges zombie flag, which is the honest reading: the mandate is fading, not gone, and the arrangement persists on institutional momentum plus real coordination utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the marketplace_pidgin_reading of the kernel hebrew_linguistic_life; how would epsilon and the party structure change under a sibling reading of the same kernel?',
    'Authoring the sibling stories (liturgical_preservation_reading, native_generational_reading) against the same referent and comparing: the referent, the standing death-and-revival regime, stays fixed while epsilon, beneficiaries, and victims re-derive from each reading''s own criterion of linguistic life.',
    'Under the liturgical reading the standing regime looks nearly benign (epsilon near zero, since it faithfully transmits sacred texts), collapsing the extraction finding; under the native-generational reading epsilon rises further and even modern spoken Hebrew''s status destabilizes. Cross-reading comparison, not this file alone, carries the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed epsilon over a shared kernel; sibling files carry the structural delta.').

omega_variable(
    pidgin_extent_and_stability,
    'How extensive, stable, and functionally central was pre-1880 spoken Hebrew in Jerusalem and the wider Sephardi diaspora: a working inter-communal medium, or occasional gesture-level contact talk?',
    'Systematic collation of nineteenth-century traveler diaries, consular reports, rabbinical-court records, and merchant correspondence, with quantitative assessment of domains, frequency, and stability of the spoken variety.',
    'Extensive stable use confirms this reading''s corrective and raises the standing regime''s measured extraction; marginal use shrinks the corrective and pushes the overall story back toward the revival narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_extent_and_stability, empirical, 'Empirical extent of the pre-revival spoken Hebrew continuum.').

omega_variable(
    gatekeeping_versus_neglect,
    'Is the marginality of continuity scholarship in school curricula produced by active gatekeeping (committee vetoes, syllabus politics) or by benign neglect (source difficulty, disciplinary habit)?',
    'Committee and editorial minutes, syllabus revision records, and citation-network analysis comparing what is assigned in schools versus what is published in journals.',
    'Active gatekeeping raises the suppression score and tilts the arrangement toward pure extraction; benign neglect lowers it and supports the hybrid coordination-plus-extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_versus_neglect, empirical, 'Mechanism behind the continuity record''s curricular marginality.').

omega_variable(
    recognition_extraction_normativity,
    'Does uncredited historical recognition constitute extraction at all, given that the continuity communities practiced their usage as ordinary life rather than as a claim needing defense?',
    'Value-theoretic analysis combined with community testimony: whether the communities, once shown the misattribution, experience it as a loss requiring remedy.',
    'If recognition is not a good anyone was deprived of, the transfer function reduces to scholarly error correction and the arrangement reads closer to pure coordination; if it is, the extraction finding stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recognition_extraction_normativity, preference, 'Whether narrative-credit appropriation counts as extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 0, 135).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t0, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t0, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t15, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t15, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t30, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t30, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t45, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t45, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t60, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 60, 0.44).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t60, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t75, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 75, 0.47).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t75, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t90, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 90, 0.46).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t90, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t105, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 105, 0.48).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t105, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t120, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 120, 0.49).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t120, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_tr_t135, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 135, 0.5).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_tr_t135, observed).

% Extraction over time
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t0, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t0, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t15, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t15, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t30, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t30, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t45, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 45, 0.63).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t45, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t60, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t60, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t75, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t75, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t90, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 90, 0.68).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t90, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t105, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 105, 0.66).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t105, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t120, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 120, 0.63).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t120, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_be_t135, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 135, 0.6).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_be_t135, observed).

% Suppression requirement over time
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t0, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t0, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t15, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t15, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t30, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t30, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t45, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 45, 0.63).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t45, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t60, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t60, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t75, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 75, 0.66).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t75, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t90, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 90, 0.6).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t90, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t105, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 105, 0.55).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t105, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t120, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 120, 0.52).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t120, observed).
narrative_ontology:measurement(heb_life_mkt_pidgin_su_t135, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 135, 0.5).
narrative_ontology:measurement_basis(heb_life_mkt_pidgin_su_t135, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'was Hebrew revived from the dead?' conflates three structurally distinct claims about what linguistic life consists in. Per the epsilon-invariance principle it decomposes into a three-member family sharing the kernel hebrew_linguistic_life: liturgical_preservation_reading (life as unbroken recitation and study of sacred texts), marketplace_pidgin_reading (this file: life as inter-communal practical function), and native_generational_reading (life as mother-tongue acquisition across all daily functions). Each member carries its own epsilon, beneficiaries, and victims over the same standing referent. The upstream liturgical reading is typically cited as evidence by the other two, hence the network edges; this file's epsilon (0.60) sits between the liturgical reading's near-zero and the native-generational reading's higher value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
