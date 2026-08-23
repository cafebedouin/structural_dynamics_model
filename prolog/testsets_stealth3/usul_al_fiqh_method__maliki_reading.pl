% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Source Hierarchy: Medinan Practice, Public Interest, and Custom as Legal Sources
 *   domain: religious/legal-theoretic
 *
 * SUMMARY:
 *   A dominant reading of Islamic legal methodology, held by the Maliki
 *   school, ranks the sources of law so that the continuous communal practice
 *   of Medina — the Prophet's own city — carries evidentiary weight in its
 *   own right alongside transmitted reports; public interest not tied to any
 *   specific text counts as a valid source; and local custom is woven into
 *   rulings wherever it does not contradict revelation. The arrangement
 *   solved a real evidentiary crisis and let law speak the idiom of the
 *   communities it governed, but it also consolidated a proprietary asset: a
 *   source ranking only the school's own formation can operate, displacing
 *   report-grading textualists wherever its courts hold, and opening a
 *   public-interest channel that rulers and aligned jurists have repeatedly
 *   drawn on. Per the epsilon-invariance principle this file authors only the
 *   maliki_reading of the usul_al_fiqh_method kernel; the hanafi, shafii, and
 *   hanbali readings are separate constraints linked through the network
 *   section. The epsilon referent is the standing Maliki source-hierarchy
 *   arrangement itself, assessed by the reading's own lights — not the
 *   textualist alternative it competes with. The claimed type and the
 *   authored metrics are independent facts: the claim asserts genuine
 *   coordination bound to asymmetric extraction; the metrics describe the
 *   arrangement's actual operation. KEY AGENTS (by structural relationship):
 *   - maliki_scholarly_establishment: agenda-setting seat
 *   (institutional/identity_locked) — administers the source ranking through
 *   teaching, fatwa, and judicial training - medinan_practice_households:
 *   beneficiary (organized/constrained) — Medinan scholarly lines whose
 *   communal practice carries evidentiary weight -
 *   maghribi_customary_communities: beneficiary (moderate/constrained) —
 *   North African and Andalusian communities whose customs gain juridical
 *   force - maghribi_ruling_elites: beneficiary with agenda-setting reach
 *   (powerful/arbitrage) — dynasties appointing qadis and drawing on the
 *   public-interest channel - textualist_hadith_specialists: primary target
 *   (powerful/constrained) — report-grading scholars whose method loses
 *   adjudicative territory - subjects_of_maslahah_rulings: primary target
 *   (powerless/trapped) — individuals ruled against on interest or custom
 *   grounds without a seat in the derivation - custom_disputant_minorities:
 *   excluded voice (powerless/trapped) — communities whose differing
 *   practices were overridden when one region's custom was canonized -
 *   comparative_law_analysts: analytical observer — sees all four readings
 *   side by side
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.45).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.42).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Source Hierarchy: Medinan Practice, Public Interest, and Custom as Legal Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "religious/legal-theoretic").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '3ad829ca-96bc-426c-bc1c-ae3eb58e966d').
narrative_ontology:cs_kernel_codification('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', formalized).
narrative_ontology:cs_authority_grounding('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', lineage).
narrative_ontology:cs_interpretation_layer_present('3ad829ca-96bc-426c-bc1c-ae3eb58e966d').
narrative_ontology:cs_reading_relation('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', usul_al_fiqh_method__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', foundational, continuous_communal_practice_outweighs_isolated_reports).
narrative_ontology:cs_axiom_status(continuous_communal_practice_outweighs_isolated_reports, holdable).
narrative_ontology:cs_axiom_grounding('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', continuous_communal_practice_outweighs_isolated_reports, empirically_contingent).
narrative_ontology:cs_axiom('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', foundational, public_interest_unbound_by_specific_text_is_legitimate_source).
narrative_ontology:cs_axiom_status(public_interest_unbound_by_specific_text_is_legitimate_source, holdable).
narrative_ontology:cs_axiom_grounding('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', public_interest_unbound_by_specific_text_is_legitimate_source, instrumental).
narrative_ontology:cs_axiom('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', secondary, custom_binds_where_not_contradicting_revelation).
narrative_ontology:cs_axiom_status(custom_binds_where_not_contradicting_revelation, holdable).
narrative_ontology:cs_axiom_grounding('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', custom_binds_where_not_contradicting_revelation, conventional).
narrative_ontology:cs_reference_frame('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', living_prophetic_city_practice_anchored_derivation).
narrative_ontology:cs_drift_state('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', classical_post_medina_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ad829ca-96bc-426c-bc1c-ae3eb58e966d', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_scholarly_establishment).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_practice_households).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maghribi_customary_communities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maghribi_ruling_elites).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, textualist_hadith_specialists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, subjects_of_maslahah_rulings).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, medinan_practice_as_independent_evidence).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, maslaha_mursala_validity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, urf_integration_where_not_contradicting_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches the school's ranked order of legal sources, trains judges and muftis in it, and issues rulings that weigh continuous Medinan communal practice, authenticated reports, public interest, and local custom. Its scholarly authority, endowed posts, and institutional continuity rest on maintaining this ranking against rival rankings; leaving it would mean abandoning the formation, networks, and offices built on it.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_scholarly_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).

% Scholarly lines of Medina whose city's continuous communal practice — how the Prophet's own community prayed, traded, married, and punished — is cited as evidence in its own right. Their standing rises whenever the practice-evidence doctrine is invoked; they transmit and curate the practice record on which later citations draw.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_practice_households, beneficiary,
    organized, generational, constrained, local).

% Muslim farming, trading, and tribal communities of North Africa and al-Andalus whose marriage forms, market habits, and land arrangements are absorbed into rulings through the custom clause. They receive law that already speaks their local idiom; their ability to leave the jurisdiction is limited by poverty, kinship, and faith.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maghribi_customary_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Dynasties and governors who appoint qadis, endow schools, and occasionally invoke public interest to justify fiscal or disciplinary measures. Patronage of the school buys them legal legitimacy, and the public-interest channel offers warrant for actions the texts do not command — though jurists can also blockade them by citing text, and they retain the option of shifting patronage between schools.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maghribi_ruling_elites, beneficiary,
    powerful, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, maghribi_ruling_elites, agenda_setter).

% Scholars whose craft is collecting, grading, and deriving from transmitted reports. Wherever the practice-and-custom ranking governs courts, their method yields to communal practice and juristic discretion they do not control. They can argue, publish, teach, and migrate between courts, but taking up the practice-evidence doctrine would mean abandoning their own methodological identity and livelihood.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, textualist_hadith_specialists, payer,
    powerful, biographical, constrained, continental).

% Individuals, debtors, taxpayers, and families judged under rulings justified by public interest or local custom rather than explicit text. They bear the outcome directly, hold no seat in the derivation, and their practical exits — appeal, flight, moving the dispute to another venue — are narrow.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, subjects_of_maslahah_rulings, payer,
    powerless, immediate, trapped, local).

% Communities whose own differing practices lost out when one region's customs were canonized into rulings — minority villages, dissenting tribes, urban quarters whose norms were overridden by the integrated standard. They are governed by a law that cites custom without their custom counting, and they have no seat in the conversation that selects which custom binds.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, custom_disputant_minorities, excluded,
    powerless, biographical, trapped, local).

% Historians and legal theorists comparing the four source hierarchies side by side. They collect no revenue and bear no rulings; their analyses circulate outside the schools' enforcement structures and are the only seat positioned to see all four readings as variants of one underlying question.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_law_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__maliki_reading, maliki_scholarly_establishment).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Answers, once and stably for a whole legal community, the question every judge faces — what counts as evidence and in what order of weight — so rulings converge across a vast region without requiring a transmitted text for every case. Continuous communal practice covers the Prophet's own norms where reports are scattered; public interest covers cases the texts never addressed; custom lets law speak the population's existing idiom.
% TRANSFER_FUNCTION: Moves adjudicative authority toward locally embedded practice and away from report-grading specialists: rulings, offices, and scholarly prestige flow to jurists formed in the school's ranking; discretionary warrant flows to rulers and jurists through the public-interest channel; legal recognition flows to majority local custom. The transfer is paid for by the displaced jurisdiction of textualist method and by the subjects of non-textual rulings.
% ABSENT_VOICES: Custom-disputant minorities whose differing practices were overridden sit outside the conversation entirely; textualist critics are heard but their objections carry no adjudicative weight inside the school's courts; and the subjects of public-interest rulings have no seat at the moment the interest is weighed.
% DISAPPEARANCE_RATIONALE: Overnight loss of the practice-public-interest-custom ranking would strip centuries of Maghribi and West African rulings of their derivational anchor: marriages, markets, and land tenures adjudicated by custom would need re-derivation under some rival ranking, the school's teaching networks would lose their core asset, and rulers would lose the public-interest warrant channel — the region's legal order would reorganize around textualist or analogical method.
% FOUNDING_PROBLEM: Early legal derivation faced evidence pointing in different directions: scattered transmitted reports contradicted one another and sometimes the continuous practice of the Prophet's own community, and the texts were silent on most of life. Something had to rank the evidence and fill the silence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: al-Shafi'i's own works (al-Risala, Kitab al-Umm) document the conflicting-evidence problem while rejecting the Medinan-practice solution, and hadith-critical scholarship classical and modern attests both the report-conflict problem and the silence problem. Whether the founding problem remains live is disputed: the school's jurists attest it never closes, while textualist and modernist reformers attest that the specific Medinan-practice resolution addresses a condition — a living prophetic-city practice — that no longer obtains.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).
:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.45 because the arrangement's costs fall on identifiable seats — textualist specialists lose adjudicative territory wherever the ranking governs, and subjects bear rulings warranted by interest or custom rather than text — while its benefits (law matched to local life, silence filled without invention) are real and diffuse. Suppression at 0.42 reflects in-school conformity plus historical state enforcement of the school's monopoly, bounded by the fact that all four readings coexist across the scholarly world. Theater at 0.30 rises across the interval as Medina's living practice recedes and citations of it become reconstructive. Accessibility_collapse at 0.50: inside the school's institutions the rival rankings collapse; across the wider scholarly world they persist. Resistance at 0.60: sustained textualist critique from al-Shafi'i's own debates onward, renewed by modern anti-taqlid movements. All three tracked metric series share one eight-point grid (t = 0 to 640, roughly 760 to 1400 CE). The suppression_requirement series is authored deliberately because the story's central dynamic is enforcement-capacity change: debate-era contention hardening under Fatimid persecution, peaking under Almoravid and Almohad state enforcement, then decaying into routine institutional conformity afterward.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the establishment seat the ranking is fidelity: the Prophet's community knew the revelation best, and honoring its practice honors the sources. From the textualist seat the same ranking is closure: a method that answers every case before the reports are consulted. From the subject seat it is unaccountable judgment: rulings arrived at through interest or custom the subject never agreed to and cannot inspect. The engine derives these divergences from the declared roles, power, and exit options; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the establishment (collects interpretive authority directly, identity-locked exit) sits nearest the beneficiary end; Medinan practice households and customary communities collect standing and idiomatic law at low cost; ruling elites collect the public-interest warrant channel but also bear juristic blockades, placing them low but not lowest. Victim declarations drive high directionality: textualist specialists bear displaced jurisdiction with constrained exit; subjects of interest-based rulings are trapped at the full-target end. No directionality overrides are authored: the derivation chain from roles, power, and exit reproduces the true relationships, and the override surface keys on power_atom alone — our two powerful seats (ruling elites near the beneficiary end, textualist specialists near the target end) sit at opposite extremes, so any keyed override would corrupt one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — conflicting evidence and textual silence — is perennial and contested rather than dead, so the arrangement carries no dead-mandate zombie signature; the mismatch consumer reads status=contested against verdict=world_rearranges and finds no capture flag. Atrophy risk is limb-specific rather than whole-constraint: the Medinan-practice limb drifts toward reconstructive citation as the living practice it names receded (visible in the rising theater_ratio series), while the public-interest and custom limbs remain load-bearing. Classifying the arrangement as pure rope would erase the enforcement asymmetry — the displaced textualist jurisdiction and the elite-drawn interest channel are real; classifying it as snare would erase the coordination function — the ranking genuinely solved the evidentiary crisis and genuinely integrated beneficial custom. The tangled_rope claim holds both facts and lets the engine price the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_usul_sources,
    'This constraint is one reading of the usul_al_fiqh_method kernel — the maliki_reading, granting independent evidentiary weight to continuous communal practice and broad non-textual sources. What would the sibling readings change structurally?',
    'Comparative classification of the sibling stories (hanafi, shafii, hanbali readings) over the same referent: the source-hierarchy arrangement each instantiates.',
    'Under the shafii reading the beneficiary/victim structure inverts — report authentication becomes the protected center and Medinan practice loses evidentiary standing; under the hanbali reading the public-interest channel closes and custom integration narrows; the epsilon and classification of THIS story are stable only within the Maliki frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_usul_sources, conceptual, 'Committer structure: one reading of a four-reading kernel; the disagreement is located in the evidentiary status of communal practice and the reach of non-textual sources.').

omega_variable(
    amal_living_practice_recension,
    'After Medina ceased to host a living, self-understood communal legal practice (by roughly the third Islamic century), does the practice-evidence doctrine still carry the force it claims, or does it operate on a curated reconstruction of that practice?',
    'Compare rulings issued where documented Medinan practice exists against rulings where the practice is asserted through transmission claims alone; measure citation patterns and agreement rates.',
    'If the doctrine runs on reconstruction, the theater_ratio understates performative content and the Medinan-practice limb drifts toward inertial maintenance while the public-interest and custom limbs carry the live function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_living_practice_recension, empirical, 'Whether the Medinan-practice limb operates on living practice or curated memory.').

omega_variable(
    maslaha_discipline_or_channel,
    'In operation, is the public-interest source a disciplined juristic instrument (genuine, universal interest, not contradicting definitive texts, as al-Shatibi codified) or an open channel through which rulers and aligned jurists license measures lacking textual warrant?',
    'Audit public-interest-invoked rulings across the classical and modern record for alignment between the invoked interest and the interests of the issuing elite.',
    'If the channel dominates, effective extraction exceeds the authored scalar and the arrangement carries a snare-shaped component riding on the coordination function; if disciplined, the authored extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_discipline_or_channel, empirical, 'Whether the public-interest source functions as disciplined tool or elite license.').

omega_variable(
    urf_contradiction_boundary_discretion,
    'The custom clause admits local practice ''where not contradicting text'' — who decides contradiction, and does that boundary move under social or political pressure?',
    'Track fatwa and court decisions where custom-text tension arises; determine whether the contradiction call tracks methodological criteria or the custom''s social entrenchment.',
    'If the boundary is discretion-managed, the custom limb transfers adjudicative power to whoever controls the contradiction judgment, concentrating extraction in the establishment seat above the authored scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urf_contradiction_boundary_discretion, conceptual, 'Whether the custom-integration boundary is criterion-governed or discretion-governed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 640).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(usul_tr_t0, observed).
narrative_ontology:measurement(usul_tr_t90, usul_al_fiqh_method__maliki_reading, theater_ratio, 90, 0.12).
narrative_ontology:measurement_basis(usul_tr_t90, observed).
narrative_ontology:measurement(usul_tr_t180, usul_al_fiqh_method__maliki_reading, theater_ratio, 180, 0.14).
narrative_ontology:measurement_basis(usul_tr_t180, observed).
narrative_ontology:measurement(usul_tr_t270, usul_al_fiqh_method__maliki_reading, theater_ratio, 270, 0.17).
narrative_ontology:measurement_basis(usul_tr_t270, observed).
narrative_ontology:measurement(usul_tr_t360, usul_al_fiqh_method__maliki_reading, theater_ratio, 360, 0.21).
narrative_ontology:measurement_basis(usul_tr_t360, observed).
narrative_ontology:measurement(usul_tr_t450, usul_al_fiqh_method__maliki_reading, theater_ratio, 450, 0.25).
narrative_ontology:measurement_basis(usul_tr_t450, observed).
narrative_ontology:measurement(usul_tr_t540, usul_al_fiqh_method__maliki_reading, theater_ratio, 540, 0.28).
narrative_ontology:measurement_basis(usul_tr_t540, observed).
narrative_ontology:measurement(usul_tr_t640, usul_al_fiqh_method__maliki_reading, theater_ratio, 640, 0.3).
narrative_ontology:measurement_basis(usul_tr_t640, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(usul_be_t0, observed).
narrative_ontology:measurement(usul_be_t90, usul_al_fiqh_method__maliki_reading, base_extractiveness, 90, 0.31).
narrative_ontology:measurement_basis(usul_be_t90, observed).
narrative_ontology:measurement(usul_be_t180, usul_al_fiqh_method__maliki_reading, base_extractiveness, 180, 0.35).
narrative_ontology:measurement_basis(usul_be_t180, observed).
narrative_ontology:measurement(usul_be_t270, usul_al_fiqh_method__maliki_reading, base_extractiveness, 270, 0.4).
narrative_ontology:measurement_basis(usul_be_t270, observed).
narrative_ontology:measurement(usul_be_t360, usul_al_fiqh_method__maliki_reading, base_extractiveness, 360, 0.43).
narrative_ontology:measurement_basis(usul_be_t360, observed).
narrative_ontology:measurement(usul_be_t450, usul_al_fiqh_method__maliki_reading, base_extractiveness, 450, 0.44).
narrative_ontology:measurement_basis(usul_be_t450, observed).
narrative_ontology:measurement(usul_be_t540, usul_al_fiqh_method__maliki_reading, base_extractiveness, 540, 0.45).
narrative_ontology:measurement_basis(usul_be_t540, observed).
narrative_ontology:measurement(usul_be_t640, usul_al_fiqh_method__maliki_reading, base_extractiveness, 640, 0.45).
narrative_ontology:measurement_basis(usul_be_t640, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(usul_su_t0, observed).
narrative_ontology:measurement(usul_su_t90, usul_al_fiqh_method__maliki_reading, suppression_requirement, 90, 0.26).
narrative_ontology:measurement_basis(usul_su_t90, observed).
narrative_ontology:measurement(usul_su_t180, usul_al_fiqh_method__maliki_reading, suppression_requirement, 180, 0.38).
narrative_ontology:measurement_basis(usul_su_t180, observed).
narrative_ontology:measurement(usul_su_t270, usul_al_fiqh_method__maliki_reading, suppression_requirement, 270, 0.5).
narrative_ontology:measurement_basis(usul_su_t270, observed).
narrative_ontology:measurement(usul_su_t360, usul_al_fiqh_method__maliki_reading, suppression_requirement, 360, 0.6).
narrative_ontology:measurement_basis(usul_su_t360, observed).
narrative_ontology:measurement(usul_su_t450, usul_al_fiqh_method__maliki_reading, suppression_requirement, 450, 0.54).
narrative_ontology:measurement_basis(usul_su_t450, observed).
narrative_ontology:measurement(usul_su_t540, usul_al_fiqh_method__maliki_reading, suppression_requirement, 540, 0.47).
narrative_ontology:measurement_basis(usul_su_t540, observed).
narrative_ontology:measurement(usul_su_t640, usul_al_fiqh_method__maliki_reading, suppression_requirement, 640, 0.42).
narrative_ontology:measurement_basis(usul_su_t640, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, information_standard).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Islamic legal methodology' covers four structurally distinct source hierarchies; per the epsilon-invariance principle each reading is authored as its own constraint with its own epsilon, beneficiaries, and victims. This file authors only the maliki_reading. Family links run to the hanafi, shafii, and hanbali readings. The shafii systematization of legal methodology as a meta-discipline is upstream of all later articulations, including this reading's mature self-description; this reading's practice-elevation and public-interest channels exert downstream pressure on the others' boundary conditions. The sharpest edge is to the shafii reading, whose core premise (authenticated reports as the exclusive root of derivation) directly contradicts this reading's foundational axiom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
