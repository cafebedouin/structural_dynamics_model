% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia as Sovereign Islamic Worship Space (1453 Conquest / Waqf Reading)
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the ISLAMIC SOVEREIGNTY reading of the Hagia
 *   Sophia kernel: the claim that the 1453 Ottoman conquest and Mehmed II's
 *   waqf endowment created a perpetual, legally irrevocable Islamic religious
 *   character for the site, such that the 1934 secular museum designation was
 *   a temporary and ultra vires suspension rather than a legitimate
 *   re-founding. Under this reading, the 2020 Council of State ruling and
 *   presidential decree are a correction, not a rupture. This is a distinct
 *   constraint from the sibling readings — the orthodox_restitution_reading
 *   and universal_heritage_reading are separate stories with their own
 *   beneficiary/victim structures and their own epsilon; they are not
 *   alternate measurements of this same constraint. The extraction climbs
 *   sharply after 2016-2020 as the political project to reconvert the site
 *   moved from rhetoric to executed legal and administrative action.
 *
 * KEY AGENTS:
 *   - akp_political_coalition: agenda_setter/institutional — drove and now administers the reconversion
 *   - turkish_islamic_constituency: beneficiary/organized — gains restored worship access and symbolic vindication
 *   - non_muslim_visitors: payer/powerless — bear curtailed access and altered viewing terms
 *   - unesco_heritage_regime: excluded/institutional — denied consultative jurisdiction it nominally holds
 *   - secularist_turks: payer/moderate — experience the reversal as ideological defeat
 *   - turkish_state_judiciary: agenda_setter/institutional — supplied the legal instrument enabling reconversion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.55).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia as Sovereign Islamic Worship Space (1453 Conquest / Waqf Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, 'c4686f64-d5de-4350-82b8-2cb47e66dbe5').
narrative_ontology:cs_kernel_codification('c4686f64-d5de-4350-82b8-2cb47e66dbe5', distributed).
narrative_ontology:cs_authority_grounding('c4686f64-d5de-4350-82b8-2cb47e66dbe5', extraction).
narrative_ontology:cs_interpretation_layer_present('c4686f64-d5de-4350-82b8-2cb47e66dbe5').
narrative_ontology:cs_reading_relation('c4686f64-d5de-4350-82b8-2cb47e66dbe5', hagia_sophia_substrate__orthodox_restitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4686f64-d5de-4350-82b8-2cb47e66dbe5', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('c4686f64-d5de-4350-82b8-2cb47e66dbe5', foundational, waqf_endowment_creates_perpetual_irrevocable_religious_designation).
narrative_ontology:cs_axiom_status(waqf_endowment_creates_perpetual_irrevocable_religious_designation, holdable).
narrative_ontology:cs_axiom_grounding('c4686f64-d5de-4350-82b8-2cb47e66dbe5', waqf_endowment_creates_perpetual_irrevocable_religious_designation, conventional).
narrative_ontology:cs_axiom('c4686f64-d5de-4350-82b8-2cb47e66dbe5', secondary, conquest_establishes_valid_sovereign_title_over_sacred_sites).
narrative_ontology:cs_axiom_status(conquest_establishes_valid_sovereign_title_over_sacred_sites, holdable).
narrative_ontology:cs_axiom_grounding('c4686f64-d5de-4350-82b8-2cb47e66dbe5', conquest_establishes_valid_sovereign_title_over_sacred_sites, conventional).
narrative_ontology:cs_reference_frame('c4686f64-d5de-4350-82b8-2cb47e66dbe5', ottoman_conquest_perpetual_waqf).
narrative_ontology:cs_drift_state('c4686f64-d5de-4350-82b8-2cb47e66dbe5', post_2020_decree_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('c4686f64-d5de-4350-82b8-2cb47e66dbe5', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, waqf_perpetuity_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, conquest_derived_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drove the 2020 Council of State ruling that annulled the 1934 secularization decree and the subsequent presidential decree reconverting the site to a mosque. Administers the site's re-designation as waqf property under the Diyanet (Directorate of Religious Affairs), sets prayer schedules, and controls tourist access windows. Collects durable political capital with its religious-nationalist base each election cycle.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, beneficiary).

% Gains a long-sought restoration of what it regards as rightfully consecrated Islamic worship space, reversing what it experienced as a century of secularist erasure. Can now pray inside the building rather than at its periphery. Exit is not really at issue for this group — the constraint's operation directly satisfies its preference.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% A diffuse, non-organized symbolic beneficiary: the reconversion is presented internationally as a restoration of Islamic sovereignty over a historically significant conquest site, generating prestige and legitimacy signaling for Islamic governance more broadly, without any single actor collecting rents on its behalf.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).

% Tourists and pilgrims of other faiths (particularly Orthodox Christians visiting what was the seat of Byzantine Christianity) now visit under altered terms: free public hours are curtailed around prayer times, iconography is covered or curtained during worship, and the site's byzantine mosaics are less continuously viewable. They can decline to visit, but the site's unique historical significance makes that a real cost, not a free substitution.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, constrained, global).

% As a UNESCO World Heritage Site, Hagia Sophia's management is nominally subject to international heritage-preservation norms and consultation expectations. Turkey's 2020 decree was issued without prior UNESCO consultation, denying the international heritage regime any binding say over the reclassification. UNESCO expressed 'regret' but has no enforcement mechanism against a sovereign state's domestic legal reversal.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime, excluded,
    institutional, generational, trapped, global).

% Turks who trace their political identity to Atatürk's 1934 secularization of the site as a museum experience the reconversion as a direct ideological defeat and a rollback of Kemalist state secularism. They can protest or litigate domestically, but the Council of State ruling and executive decree have already foreclosed the legal avenue; remaining options are electoral opposition over a multi-year horizon.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    moderate, biographical, constrained, national).

% The Council of State provided the legal instrument — ruling the 1934 museum decree exceeded Atatürk's authority because the site had been irrevocably designated a mosque waqf by Mehmed II in 1453. This retroactive doctrinal finding is the load-bearing legal argument for the entire reading; the court's independence from the executive that benefits from its ruling is itself contested.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legally stabilized answer to 'what is this building for' that ends decades of ambiguous museum-status limbo, allowing the state to coordinate religious practice, tourism management, and heritage conservation under one clear administrative authority (the Diyanet) rather than split jurisdiction.
% TRANSFER_FUNCTION: Moves symbolic and practical control of the site from secular/international heritage administration to Turkish Islamic religious authority; moves uninterrupted, faith-neutral access from all visitors to conditional access for non-Muslim visitors; moves diplomatic goodwill and international heritage-regime standing from Turkey's international relationships to its domestic religious-nationalist coalition.
% ABSENT_VOICES: The Ecumenical Patriarchate of Constantinople and Greek Orthodox Christians worldwide, for whom the site is the historical seat of Eastern Christianity, were not party to the domestic legal proceeding that reclassified it. UNESCO's advisory committees were not consulted prior to the decree. Both would object to the conquest-derived sovereignty premise on principle, not merely on access terms.
% DISAPPEARANCE_RATIONALE: If the 2020 reconversion were reversed and museum status restored, prayer schedules and access curtailments would end, Diyanet administrative control would be replaced by cultural-ministry museum management, UNESCO consultation processes would resume normal standing, and the AKP coalition would lose a significant symbolic asset heading into future elections — the arrangement is actively load-bearing for multiple parties, not a naturalized background fact.
% FOUNDING_PROBLEM: The claimed founding problem is the 1934 secularization decree's alleged legal defect: that Atatürk lacked authority to dissolve a perpetual Islamic waqf established by Mehmed II upon the 1453 conquest, and that the mosque's true, uninterrupted legal status as consecrated Islamic endowment was simply suppressed for 86 years.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish Council of State and the Diyanet attest the waqf-perpetuity claim as settled doctrine. Independent legal historians and comparative waqf-law scholars outside Turkey's judiciary dispute the retroactive application of Ottoman-era endowment law to override a later sovereign secular state's decree, and international heritage law scholars characterize the 1934 museum designation as itself a legitimate and durable act of a sovereign successor state — corroboration for the 'problem' framing exists almost entirely within the benefiting institutions themselves.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.62) reflecting political consolidation value captured by the AKP coalition, the diplomatic friction cost imposed on Turkey's international relationships, and the access costs borne by non-Muslim visitors and the international heritage regime — but it is not extreme because the site remains physically open to the public and no direct financial extraction occurs. Suppression (0.55) reflects that alternatives (restoring museum status, international co-management) have been foreclosed through executive decree and a court ruling that itself post-hoc validated the political outcome, not through violence or economic coercion — this is legal-institutional suppression of an alternative sovereignty framing. Theater ratio (0.40) is meaningfully elevated because much of the reconversion's public presentation (ceremonial re-opening, carefully staged coverage) serves identity-signaling and political-consolidation functions rather than pure religious administration.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP coalition and the judiciary that supplied its legal instrument sit at the beneficiary/agenda-setter end — the arrangement enhances their institutional and political power and they face essentially no exit cost. The Turkish Islamic constituency is a genuine beneficiary whose long-standing preference is satisfied. Non-Muslim visitors and secularist Turks sit at the target end: they bear the access curtailment and the ideological defeat respectively, and their exit options are constrained (visiting is costly to forgo; domestic political opposition is a multi-year project foreclosed in the near term by the ruling itself). UNESCO is excluded outright — its nominal jurisdiction was never exercised because the decree bypassed the consultative mechanism that would trigger it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem framing (that 1934 was itself the illegitimate suspension of a perpetual waqf) is precisely the kind of genealogy claim that must not be taken as self-corroborating: it is asserted almost entirely by the parties who benefit from its acceptance (the judiciary that ruled it, the executive that acted on the ruling). The Q5 disappearance verdict (world_rearranges) combined with a contested founding-problem status flags this as an active, load-bearing political-religious project rather than a dormant natural fact — exactly the mismatch pattern the R5 genealogy interview exists to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_perpetuity_doctrine_validity,
    'Does Ottoman-era waqf endowment law create a legally perpetual religious designation that a later sovereign secular state cannot lawfully override, or is the 1934 secularization a legitimate exercise of successor-state sovereignty that fully supersedes the Ottoman-era endowment?',
    'Comparative analysis by independent (non-Turkish-state) legal historians of how waqf perpetuity claims have been treated in other former Ottoman territories facing analogous successor-state secularization, plus international legal scholarship on state succession and religious-property doctrine.',
    'If waqf perpetuity is genuinely binding on successor states, the islamic_sovereignty_reading''s legal foundation is a defensible reading of an actual, low-ε constraint (closer to a Rope enforcing a genuinely prior claim). If not, the entire legal apparatus (Council of State ruling, decree) is a constructed justification for a extraction-heavy political consolidation, raising effective epsilon further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waqf_perpetuity_doctrine_validity, conceptual, 'Whether Ottoman waqf perpetuity doctrine can lawfully bind Turkey''s later secular state, or is a retroactive legal fiction.').

omega_variable(
    judicial_independence_of_reversal_ruling,
    'Was the 2020 Council of State ruling an independent judicial finding reached on its own legal merits, or was it a coordinated outcome aligned with executive political preference (i.e., judicial capture)?',
    'Comparative timeline analysis of the ruling relative to AKP''s public reconversion advocacy, judicial appointment patterns on the relevant court panel, and comparison with the court''s independence record in other politically salient cases during the same period.',
    'If the ruling reflects genuine judicial independence, the constraint''s authority_grounding is closer to legitimate legal process; if the ruling was coordinated with executive preference, the entire legal legitimation is theater dressing extraction as adjudication, which would push the effective classification further toward snare at the judiciary seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_of_reversal_ruling, empirical, 'Whether the enabling court ruling reflects independent adjudication or coordinated political outcome.').

omega_variable(
    sibling_reading_framing_selection,
    'Given three competing readings of the same physical site (Islamic sovereignty, Orthodox restitution, universal heritage), what determined which reading this story instantiates as opposed to treating the site as a single constraint with contested measurement?',
    'This was resolved by the ε-invariance principle: the three readings produce structurally distinct beneficiary/victim sets, distinct authority-grounding claims, and distinct epsilon trajectories, so they were authored as three separate linked constraint stories rather than one story with an ambiguous claim.',
    'Confirms this story''s epsilon (0.62) applies only to the islamic_sovereignty_reading''s specific transfer structure; the sibling readings carry their own independently authored epsilon values and must not be averaged or reconciled with this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_framing_selection, conceptual, 'Documents the kernel decomposition rationale distinguishing this reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 1934, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1934, 0.05).
narrative_ontology:measurement(hagi_tr_t1980, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(hagi_tr_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(hagi_tr_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1934, 0.1).
narrative_ontology:measurement(hagi_be_t1980, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(hagi_be_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2016, 0.35).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(hagi_be_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1934, 0.1).
narrative_ontology:measurement(hagi_su_t1980, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(hagi_su_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2016, 0.28).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(hagi_su_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2022, 0.54).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_restitution_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the hagia_sophia_substrate kernel. islamic_sovereignty_reading, orthodox_restitution_reading, and universal_heritage_reading share a physical referent but instantiate structurally distinct constraints with different beneficiary/victim sets, different authority-grounding claims, and different epsilon values. Do not average or reconcile epsilon across the three; each is authored independently per the epsilon-invariance principle and linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
