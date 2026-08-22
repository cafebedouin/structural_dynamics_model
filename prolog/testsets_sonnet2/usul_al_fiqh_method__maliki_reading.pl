% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and 'Urf as Independent Sources
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This story authors the Maliki reading of the usul al-fiqh kernel: the
 *   methodological commitment that treats the continuous,
 *   communally-transmitted practice of Medina ('amal ahl al-Madina) as
 *   independent evidence alongside hadith, admits maslaha mursala (public
 *   interest unconstrained by explicit text) as a valid source, and
 *   integrates local 'urf wherever it does not contradict text. This is a
 *   distinct constraint from the Hanafi, Shafii, and Hanbali readings of the
 *   same kernel — each reading fixes a different source hierarchy and
 *   therefore produces different legal outcomes and a different
 *   beneficiary/victim structure, even though all four readings share the
 *   underlying kernel of 'how is Islamic law derived from foundational
 *   sources.' Per the ε-invariance principle, this story does not average
 *   across readings or hedge; ε here is authored solely for the Maliki
 *   arrangement as its own adherents and critics assess it.
 *
 * KEY AGENTS:
 *   - maliki_qadis: administer and enforce the methodology in Maliki-jurisdiction courts
 *   - medinan_juristic_tradition: collects legitimacy from elevated evidentiary status of communal practice
 *   - regional_customary_authorities: benefit from 'urf integration
 *   - non_medinan_hadith_transmitters: bear discounted evidentiary weight
 *   - textualist_minority_communities: bear displacement of preferred textual rulings
 *   - litigants_outside_maliki_regions: bear jurisdictional inconsistency costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.38).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.32).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and 'Urf as Independent Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "religious/legal").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'a01b83b1-6678-4e61-9f03-25f02ce89ab4').
narrative_ontology:cs_kernel_codification('a01b83b1-6678-4e61-9f03-25f02ce89ab4', distributed).
narrative_ontology:cs_authority_grounding('a01b83b1-6678-4e61-9f03-25f02ce89ab4', lineage).
narrative_ontology:cs_interpretation_layer_present('a01b83b1-6678-4e61-9f03-25f02ce89ab4').
narrative_ontology:cs_reading_relation('a01b83b1-6678-4e61-9f03-25f02ce89ab4', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('a01b83b1-6678-4e61-9f03-25f02ce89ab4', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_reading_relation('a01b83b1-6678-4e61-9f03-25f02ce89ab4', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('a01b83b1-6678-4e61-9f03-25f02ce89ab4', foundational, continuous_communal_practice_as_independent_evidence).
narrative_ontology:cs_axiom_status(continuous_communal_practice_as_independent_evidence, holdable).
narrative_ontology:cs_axiom_grounding('a01b83b1-6678-4e61-9f03-25f02ce89ab4', continuous_communal_practice_as_independent_evidence, conventional).
narrative_ontology:cs_axiom('a01b83b1-6678-4e61-9f03-25f02ce89ab4', foundational, unrestricted_public_interest_as_valid_source).
narrative_ontology:cs_axiom_status(unrestricted_public_interest_as_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('a01b83b1-6678-4e61-9f03-25f02ce89ab4', unrestricted_public_interest_as_valid_source, instrumental).
narrative_ontology:cs_reference_frame('a01b83b1-6678-4e61-9f03-25f02ce89ab4', medinan_community_continuous_practice).
narrative_ontology:cs_drift_state('a01b83b1-6678-4e61-9f03-25f02ce89ab4', post_classical_codification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a01b83b1-6678-4e61-9f03-25f02ce89ab4', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_juristic_tradition).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_customary_authorities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_qadis).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, non_medinan_hadith_transmitters).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, textualist_minority_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, litigants_outside_maliki_regions).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, living_community_practice_as_evidence).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, public_interest_as_independent_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate disputes by weighing Medinan practice, maslaha mursala, and local 'urf alongside hadith. They administer the methodology, decide which customs count as non-contradicting, and thereby control which local practices gain the force of law. Their authority is enhanced, not diminished, by treating consensus of Medina as an independent evidentiary channel.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_qadis, agenda_setter,
    institutional, generational, arbitrage, regional).

% The tradition tracing to Malik ibn Anas and the early Medinan community collects legitimacy from having its accumulated practice treated as a direct evidentiary witness to Prophetic normativity, on par with individually transmitted hadith. This privileges a specific city's continuous practice over hadith transmitted by isolated narrators elsewhere.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_juristic_tradition, beneficiary,
    institutional, civilizational, analytical, regional).

% Local custom-holders (merchants, tribal elders, agricultural communities) benefit because their 'urf is integrated as law wherever it does not contradict explicit text. Where Maliki jurisdiction runs, local practice is not merely tolerated but elevated to a recognized source, giving customary authorities real leverage in how law is applied to them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_customary_authorities, beneficiary,
    organized, generational, constrained, regional).

% Jurists and transmitters whose authenticated hadith originate outside Medina find their reports subordinated when they conflict with established Medinan practice — the reasoning being that a widely-attested communal practice is a stronger indicator of continuous Prophetic norm than an individually transmitted report, however sound its chain. Their evidentiary contribution is structurally discounted by a rule they cannot contest from outside the tradition.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, non_medinan_hadith_transmitters, payer,
    moderate, civilizational, trapped, continental).

% Communities within Maliki jurisdictions who hold strict textualist commitments (preferring literal hadith application over customary or public-interest reasoning) find their preferred rulings displaced when local qadis invoke maslaha mursala or 'urf. They have limited standing to challenge a ruling grounded in an independent source category their own methodology does not recognize as legitimate.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, textualist_minority_communities, payer,
    powerless, generational, trapped, regional).

% Traders, migrants, and litigants who move between Maliki and non-Maliki jurisdictions face unpredictable rulings: an act licit under maslaha mursala or local 'urf in one region may be treated differently elsewhere, and they bear the transaction cost of jurisdictional inconsistency without having chosen the framework being applied to them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, litigants_outside_maliki_regions, payer,
    powerless, biographical, constrained, continental).

% Study how the four schools diverge on source hierarchy and observe the tradeoffs each reading makes between textual uniformity and local responsiveness. They document but do not adjudicate between readings.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_jurisprudence_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__maliki_reading, diffuse).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent method for deriving law in cases where explicit text is silent or ambiguous, by treating the continuous, communally-transmitted practice of Medina and the accumulated customary norms of a locality as reliable indicators of underlying legal intent — solving the problem of legal gaps without requiring speculative individual reasoning untethered from lived practice.
% TRANSFER_FUNCTION: Moves interpretive authority from individually-transmitted hadith reports (wherever they originate) toward the collective, continuous practice of a specific historical community (Medina) and toward local custom; this shifts evidentiary weight and litigation outcomes away from parties whose claims rest on non-Medinan transmission chains or on strict text-only argument, and toward parties whose position aligns with regional practice or the public-interest judgment of the presiding qadi.
% ABSENT_VOICES: Non-Medinan Companions-era transmitters and their later scholarly descendants (whose hadith might be discounted against Medinan practice) are not structurally represented within Maliki methodology's own evaluative framework; likewise, litigants who move between legal regions have no forum to contest the inconsistency the multi-source method produces, since each jurisdiction adjudicates using its own internally coherent but mutually divergent method.
% DISAPPEARANCE_RATIONALE: If Medinan-practice-as-source and maslaha mursala were withdrawn overnight, Maliki-administered regions would lose a major channel through which local custom and communal precedent enter binding law, and rulings would have to be re-derived from hadith and analogy alone — a real disruption to legal continuity in those regions. But proponents of sibling readings (particularly Shafii and Hanbali) would say the world was already misconfigured and its 'correction' would just align Maliki practice with the same textual sources they already prioritize; hence the verdict is genuinely contested between the reading's adherents and its critics.
% FOUNDING_PROBLEM: Early Medinan jurists faced legal questions where explicit Quranic or hadith text gave no direct answer, and needed a principled way to determine likely Prophetic and Companion-era practice in a city where that practice had continued visibly and communally since the Prophet's own lifetime, without relying solely on possibly fragmentary or contested individual hadith transmission.
% FOUNDING_PROBLEM_CORROBORATION: Later Maliki jurists (e.g., al-Qarafi, Ibn Rushd al-Hafid) attest the founding problem remains live wherever text is silent and local practice is unbroken. However, Shafii-tradition critics from outside the Maliki school (al-Shafi'i himself, in his critiques of 'amal ahl al-Madina in Kitab al-Umm) argued the reliance on regional consensus as an independent evidentiary category was never adequately distinguished from ordinary custom-following, and that authenticated hadith should have superseded it from the outset — a corroborating dissent from outside the beneficiary tradition.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, contested).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.38) and rises only gradually across the interval: the coordination function (resolving legal gaps via communal practice and public interest) is genuine and long-standing, but the method structurally discounts non-Medinan evidentiary claims in a way that concentrates interpretive authority in Maliki-tradition qadis and Medinan-descended jurisprudence. Suppression is moderate (0.32) — there is no coercive apparatus forcing acceptance of Maliki methodology outside its historical jurisdictions, but within Maliki-administered regions, litigants preferring strict textualism have limited recourse. Theater ratio is low (0.15) reflecting that this is a functioning, actively-used interpretive method, not a vestigial or performative one. Accessibility collapse is moderate (0.42): scholars and litigants can and do choose or migrate toward other schools, but within an established Maliki jurisdiction, exit from the applicable method is constrained by residence and legal administration.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a Maliki qadi or Medinan-tradition jurist, this methodology is a principled solution to legal silence — coordination, not extraction. From the seat of a non-Medinan hadith transmitter or a textualist litigant caught within Maliki jurisdiction, the same structure discounts their evidentiary contribution or overrides their preferred method without consent. The engine should compute these as diverging per-seat classifications from the same structural facts; this divergence is exactly the coordination/extraction hybridity the tangled_rope claim asserts.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki qadis and the Medinan tradition sit near the beneficiary end: they administer the source hierarchy and gain interpretive authority and legitimacy from it. Regional customary authorities benefit similarly, since their 'urf gains binding force. Non-Medinan hadith transmitters, textualist minorities, and cross-jurisdiction litigants sit nearer the target end: their evidentiary claims are structurally discounted or their expectations disrupted by a method they had no say in adopting and often cannot exit (trapped or constrained exit options, given that legal jurisdiction is not freely chosen by ordinary litigants).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal gaps where text is silent, in a city with unbroken communal practice) may be judged live or dead depending on whether one credits 'amal ahl al-Madina as still epistemically privileged today, centuries removed from the living community whose practice was being witnessed. Classifying this as tangled_rope rather than snare or rope prevents both over-charitable readings (treating the discounting of non-Medinan hadith as costless) and over-harsh readings (treating the whole method as pure extraction when it does solve a genuine interpretive problem for its own adherents).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_practice_epistemic_status,
    'Does the continuous practice of the historical Medinan community constitute genuinely superior evidence of Prophetic normativity compared to individually authenticated hadith, or is it a regionally-parochial custom elevated to false universal authority?',
    'No empirical resolution is possible within the tradition''s own epistemic commitments; this is a live methodological dispute among classical and contemporary jurists (al-Shafi''i''s critique in Kitab al-Umm versus Maliki responses from al-Qarafi and later scholars) turning on contested theological and historical premises about transmission reliability.',
    'If Medinan practice is genuinely superior evidence, the beneficiary structure reflects legitimate epistemic privilege rather than extraction; if it is parochial custom mistaken for universal norm, the discounting of non-Medinan hadith transmitters constitutes a structural injustice baked into the methodology''s source hierarchy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medinan_practice_epistemic_status, conceptual, 'Whether Medinan communal practice constitutes privileged evidence or elevated regional custom.').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Given that the four Sunni schools of usul al-fiqh have coexisted for over a millennium without any one displacing the others, is this coexistence a stable equilibrium reflecting genuine interpretive pluralism the tradition sanctions, or an unresolved contradiction that a more systematized meta-discipline (as Shafii''s reading claims to provide) should in principle resolve?',
    'Historical and doctrinal analysis of whether classical scholars treated inter-school divergence as sanctioned pluralism (ikhtilaf) or as an open problem awaiting resolution; examine how each school''s own usul texts characterize the legitimacy of the other schools'' methods.',
    'If pluralism is doctrinally sanctioned, the coexists_with relations to sibling readings are correct and stable; if the tradition itself treats the divergence as an open problem, some sibling relations may be better modeled as unstable coexistence trending toward influence or partial foreclosure over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'Whether inter-school methodological pluralism is a stable, sanctioned equilibrium or an unresolved tension.').

omega_variable(
    urf_contradiction_threshold_ambiguity,
    'Who determines, and by what standard, whether a given local custom ''contradicts'' explicit text such that it must be excluded from the ''urf integration channel?',
    'Survey of Maliki juristic literature (fatawa collections, qadi manuals) for the operative criteria applied historically, and whether the threshold has been applied consistently or has drifted to accommodate powerful local interests.',
    'A loosely-applied or drifting threshold would suggest the ''urf-integration channel functions partly as a discretionary tool benefiting whichever local customary authorities have standing before the qadi, increasing the extractive character of the constraint beyond what is authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urf_contradiction_threshold_ambiguity, empirical, 'Ambiguity in how the non-contradiction threshold for custom integration is actually applied in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__maliki_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__maliki_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__maliki_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__maliki_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__maliki_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__maliki_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__maliki_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__maliki_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__maliki_reading, base_extractiveness, 80, 0.37).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__maliki_reading, base_extractiveness, 100, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(usul_al_fiqh_method__maliki_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__maliki_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'usul al-fiqh methodology' per the ε-invariance principle: each Sunni school's source-hierarchy commitments constitute a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, even though all four share the same underlying kernel (how Islamic law is properly derived from foundational sources) and coexist historically without full mutual foreclosure. The Maliki reading (this story) is distinguished from Shafii by elevating Medinan practice and maslaha mursala to independent-source status where Shafii subordinates all non-hadith reasoning to prior hadith authentication; distinguished from Hanafi by grounding public-interest reasoning in the specific evidentiary weight of Medinan communal practice rather than in expansive qiyas and ra'y; distinguished from Hanbali by admitting extra-textual sources (custom, public interest, communal practice) that Hanbali methodology treats as suspect innovations to be blocked via sadd al-dhara'i.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
