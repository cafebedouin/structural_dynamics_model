% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universalist Reading: ICC Jurisdiction Transcending Consent
 *   domain: international_law/criminal_justice/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute of the International Criminal Court (1998, entered force
 *   2002) establishes a treaty framework for prosecuting genocide, crimes
 *   against humanity, and war crimes. The universalist reading interprets the
 *   Statute to grant the ICC authority that transcends state consent: the
 *   Court may prosecute nationals of non-party states when crimes occur on
 *   the territory of a state party or are referred by the UN Security
 *   Council, thereby extending universal criminal accountability across
 *   borders without requiring the accused state's participation in the
 *   treaty. This reading positions international criminal justice as a
 *   legitimate override of classical sovereignty doctrine when core crimes
 *   are at stake. The universalist reading is in structural tension with two
 *   competing interpretations: the sovereigntist reading (Rome Statute
 *   applies only to state parties and their nationals; non-party states
 *   retain exclusive jurisdiction over their own nationals) and the hybrid
 *   complementarity reading (ICC has jurisdiction only where domestic courts
 *   are unwilling or unable to prosecute, preserving primary state
 *   authority). These readings share the same Rome Statute kernel but
 *   interpret its authority-grounding and reach fundamentally differently.
 *   The universalist reading is one commitment to how the ICC's mandate
 *   should be understood and enforced.
 *
 * KEY AGENTS:
 *   - Non-Party States with Accused Nationals: Primary victims (powerless/trapped) — face ICC jurisdiction without consent; no exit option if crime occurred on party-state territory or referred by UNSC
 *   - State Parties to Rome Statute: Institutional beneficiaries (institutional/arbitrage) — benefit from ability to prosecute non-nationals and shield own citizens through cooperation; can exit (withdraw) but choose not to
 *   - International Justice Advocates and Human Rights NGOs: Organized beneficiaries (organized/mobile) — advocate for universal jurisdiction; see ICC as tool for accountability; have agency in promoting universalist reading
 *   - Victims of Core Crimes: Diffuse beneficiaries (powerless/trapped) — benefit structurally from universal jurisdiction guarantee; gain representation via ICC; cannot exit their status as victims
 *   - UN Security Council: Institutional actor with override power (institutional/arbitrage) — can refer non-party nationals to ICC; operates under Chapter VII authority; core to the universalist reading's enforcement mechanism
 *   - ICC as Institutional Body: Organizational beneficiary (institutional/constrained) — gains legitimacy and authority from universalist reading; constrained by dependency on state cooperation for arrest and prosecution
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the universalist commitment as inevitable logical consequence of prohibition norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.58).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.62).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universalist Reading: ICC Jurisdiction Transcending Consent").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/criminal_justice/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '28796e44-66c5-4aba-b4c8-8e5b48c1f5bb').
narrative_ontology:cs_kernel_codification('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', formalized).
narrative_ontology:cs_authority_grounding('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', extraction).
narrative_ontology:cs_interpretation_layer_present('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb').
narrative_ontology:cs_reading_relation('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', foundational, prohibition_norm_overrides_consent).
narrative_ontology:cs_axiom_status(prohibition_norm_overrides_consent, holdable).
narrative_ontology:cs_axiom_grounding('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', prohibition_norm_overrides_consent, deontological).
narrative_ontology:cs_axiom('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', foundational, unsc_chapter_vii_transcends_state_sovereignty).
narrative_ontology:cs_axiom_status(unsc_chapter_vii_transcends_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', unsc_chapter_vii_transcends_state_sovereignty, conventional).
narrative_ontology:cs_reference_frame('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', universal_prohibition_primacy).
narrative_ontology:cs_drift_state('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', contemporary_sovereignty_erosion_phase, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28796e44-66c5-4aba-b4c8-8e5b48c1f5bb', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_justice_advocates).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, national_sovereignty_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-PARTY STATE WITH ACCUSED NATIONALS (SNARE) — Faces ICC jurisdiction over own nationals via territorial trigger (crime committed on party state territory) or UNSC referral without having consented to the treaty. Cannot exit the universal mandate through withdrawal or non-participation; suppression takes the form of institutional fait accompli. No coordination benefit—only extraction of sovereignty claim.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__universalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STATES HOSTING TERRITORIAL CRIME SCENES (TANGLED ROPE) — Benefit from genuine coordination function (crime-scene states delegate investigation/prosecution to neutral international body, avoiding biased domestic trials). Simultaneously bear extraction: loss of prosecutorial discretion, exposure to ICC oversight, mandatory cooperation. Exit is costly (withdrawal risks legitimacy penalty, non-cooperation triggers enforcement pressure) but nominally possible for non-parties.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__universalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL JUSTICE ADVOCATES / VICTIM-STATE COALITIONS (ROPE) — See ICC jurisdiction as solving a genuine coordination problem: universal reach prevents perpetrators escaping via border transit or non-party status. Benefits include deterrence, accountability, and victim representation. Can exit (withdraw from treaty, oppose UNSC referrals) but choose not to because coordination benefits exceed costs. Organized actors with substantial political agency.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__universalist_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ICC AS TRANSITIONAL JUSTICE INSTITUTION (SCAFFOLD) — Institutionally constrained (operates under treaty framework, dependent on state cooperation). Low theater ratio reflects that ICC prosecution is substantive legal process, not ritual. Scaffold derives from the sunset logic: ICC was originally envisioned as temporary (post-Cold War tribunal-building) with implicit assumption that domestic courts would eventually absorb capacity. Has_sunset_clause rationale: as state capacity for prosecuting core crimes improves and regionalized courts mature (African Union Court, regional human rights systems), ICC's monopoly on universal jurisdiction erodes. Estimated sunset: 20-40 years as capacity distributes.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__universalist_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL SOVEREIGNTY DOCTRINE (PITON) — The classical principle of sovereign immunity and consent-based jurisdiction persists in international law theory and state practice despite erosion by ICC's universalist reach. Theater_ratio reflects the performative maintenance of 'state sovereignty' language in declarations and resolutions even as substantive practice accepts ICC override. Piton classification: sovereignty is the former dominant institutional principle now maintained through inertia and theatrical reaffirmation rather than structural enforcement.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__universalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABLE INSTITUTIONAL LOGIC (MOUNTAIN) — From civilizational scope, may see ICC's universalist jurisdiction as reflecting an inescapable feature of international order: core crimes (genocide, crimes against humanity) generate obligations that transcend consent because they threaten the legitimacy of the international system itself. This perspective frames the universalist mandate as following necessarily from the prohibition norm, not as contingent institutional choice. FALSE SUMMIT RISK: this naturalizes what is actually a contested institutional commitment. The universalist reading is ONE interpretation of Rome Statute authority, not an inevitable law of international relations.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__universalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rome_statute_jurisdiction__universalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rome_statute_jurisdiction__universalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, TR),
    TR >= 0.70.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This constraint exhibits moderate-high extraction of state sovereignty. The universalist reading allows ICC jurisdiction over non-party state nationals without consent — a substantive loss of prosecutorial discretion. However, the extraction is not maximal (snare level ≥0.66) because: (1) the coordination function is genuine (ICC prosecution does solve the problem of perpetrators escaping justice via border transit or non-party status), (2) state parties benefit from the reciprocal reach (they can prosecute non-nationals within their territory), and (3) exit is nominally possible (state parties can withdraw, though reputationally costly). The value of 0.58 reflects the mixed character: real coordination benefit (justice system that reaches across borders) coupled with asymmetric extraction (non-parties bear costs they didn't authorize). Suppression (0.62): Moderate-high. Non-party states face high barriers to exit: UNSC referral is essentially mandatory (Chapter VII authority), territorial triggers cannot be avoided if crimes occur on party territory, and reputational costs of non-cooperation with ICC are substantial. State practice shows significant pressure to cooperate with arrest warrants despite formal sovereignty claims. Suppression is not total because state parties retain formal sovereignty (can nominally refuse to arrest, though with consequences) and because the prohibition norm provides legitimacy that makes suppression feel justified rather than purely coercive. Theater ratio (0.48): Relatively low. ICC prosecutions are substantive legal proceedings with genuine evidentiary standards, witness testimony, and verdicts. Theater is reduced compared to diplomatic negotiations or UN resolutions because the judicial process imposes real constraints. However, theater is non-zero because: (1) ICC legitimacy partly depends on performative demonstrations of impartiality and fairness, (2) some prosecutions are driven by geopolitical interests (UNSC referrals are politically selective), and (3) the constraint involves rhetorical claims about 'universal justice' that may exceed the actual reach. The trajectory from 0.52 to 0.48 reflects maturation: early ICC era involved more performative legitimacy-building; current era (2010-2026) shows more substantive caseload and less performative messaging, as the Court has established institutional credibility.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps reveal how the same Rome Statute authority-claim generates different constraint types depending on structural position. Non-party states with accused nationals see snare (high extraction, no exit, no coordination benefit to them personally). State parties see rope or tangled rope (genuine coordination benefit from reciprocal reach, but some extraction via obligation to cooperate). Justice advocates see rope (pure coordination — solving the problem of perpetrator flight). ICC itself sees scaffold (temporary institutional form with sunset as national capacity builds). The analytical observer risks seeing mountain (universal jurisdiction as inevitable consequence of prohibition norms) but structural data suggests false summit (the universalist reading is one institutional choice, not a law of nature). These gaps demonstrate that the constraint is genuinely perspectival: the Rome Statute's authority structure is experienced differently by targets, beneficiaries, and observers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural position relative to extraction flow. Non-party states with accused nationals: d ≈ 0.95 (full target — experience high extraction, receive no coordination benefit from treaty). State parties: d ≈ 0.15 (net beneficiary through arbitrage — can prosecute non-nationals, benefit from reciprocal reach). Justice advocates: d ≈ 0.10 (beneficiary — see universalist reading as expanding accountability). Victims: d ≈ 0.08 (beneficiary — benefit from guarantee of accountability, though structurally unable to exit victim status). ICC institutional body: d ≈ 0.25 (partial beneficiary — gains authority from universalist reading but constrained by state cooperation requirements). UNSC: d ≈ 0.20 (institutional beneficiary — maintains discretionary override power). Analytical observer: d ≈ 0.72 (analytical position — observes full structure but risks naturalizing the universalist commitment). These d values feed the sigmoid function f(d) to produce experienced χ (effective extraction) for each perspective. Non-party states experience high χ; state parties and advocates experience low or negative χ; analytical observer experiences moderate χ reflecting the tension between coordination function and sovereignty extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The universalist reading avoids the trap of mislabeling pure extraction as coordination by explicitly declaring beneficiaries (justice advocates, victims, state parties) and victims (non-party states). The tangled_rope classification captures both the genuine coordination function (ICC solves the perpetrator-flight problem) and the asymmetric extraction (non-parties bear costs they didn't authorize). The constraint is not 'really a snare' (pure extraction) because coordination benefits are substantial and genuinely motivate the institutional design. It is not 'really a rope' (pure coordination) because non-party states experience clear extraction without benefit. Tangled rope fits because both mechanisms operate simultaneously: the same institutional mechanism (universal jurisdiction) both coordinates justice-seeking and extracts sovereignty. The mandatrophy is resolved by showing that these are not competing classifications but a single mixed structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_legitimacy_threshold,
    'At what severity threshold does the prohibition norm (universal prohibition on genocide/crimes against humanity) override the consent requirement for jurisdiction?',
    'Comparative analysis of international legal doctrine: surveying which scholars/states recognize universal jurisdiction thresholds vs. pure consent models; examining state practice in prosecuting non-nationals for core crimes; tracking evolution of regional human rights courts accepting universal jurisdiction principles',
    'If threshold is low (any serious human rights violation): universalist reading applies broadly, extractiveness rises toward snare territory. If threshold is high (only genocide/systematic mass atrocity): universalist reading is narrower, extractiveness remains in tangled_rope range. If no threshold is recognized: consent doctrine remains dominant, universalist reading is minority interpretation (sovereigntist reading becomes dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_legitimacy_threshold, conceptual, 'Consent override threshold for core crime jurisdiction').

omega_variable(
    unsc_referral_legitimacy,
    'Does UNSC referral of non-party state nationals to ICC constitute binding authority or an override mechanism lacking independent legitimacy?',
    'Examine cases where UNSC referral has been contested (e.g., Libya 2011, Darfur 2005); track state voting patterns and declarations on whether referral is seen as legitimate exercise of Chapter VII authority or as unlawful expansion of UNSC power; analyze legal scholarship consensus on whether referral requires independent authorization from the state whose nationals are referred',
    'If UNSC referral is seen as legitimate binding authority: universalist reading is strengthened (ICC can reach any state via Security Council), extraction mechanism is institutional/coercive. If referral is contested as exceeding UNSC authority: universalist reading is weakened, extraction depends on treaty party status or territorial trigger, and the constraint shifts toward hybrid complementarity reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unsc_referral_legitimacy, empirical, 'UNSC referral legitimacy and binding force').

omega_variable(
    reading_instantiation_and_sibling_coexistence,
    'Is the universalist reading internally coherent, and does it coexist with or foreclose the sovereigntist and hybrid complementarity readings within a single legal framework?',
    'Doctrinal analysis: examine whether Rome Statute text and preamble support universal jurisdiction claims; analyze case law (ICC, ICJ decisions) on whether judges operating under the universalist reading must reject sovereigntist principles; review whether state parties to Rome Statute have adopted universalist reading formally or continue to claim residual sovereignty rights; assess whether the three readings represent different hermeneutical commitments to the same kernel (Rome Statute text) or different kernels altogether',
    'If universalist reading forecloses sovereigntist within Rome Statute framework: the readings are in zero-sum competition; one legal order cannot coherently hold both. If readings coexist: the Rome Statute kernel is ambiguous enough to admit multiple readings; the constraint manifests differently under each interpretation. If coexistence is the case, this omega resolves to true, and the reading_relations should be coexists_with rather than forecloses. This affects whether the universalist reading is positioning itself as THE correct reading or as ONE legitimate interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_and_sibling_coexistence, conceptual, 'Logical structure of reading relationships within Rome Statute framework').

omega_variable(
    state_capacity_distribution,
    'As national courts and regional courts build capacity to prosecute core crimes, does ICC''s monopoly on universal jurisdiction decay or entrench?',
    'Track prosecutions of genocide/crimes against humanity by national courts (Universal Jurisdiction cases in Belgium, Spain, Netherlands, Germany) and regional courts (African Union Court, ECHR, IACHR) over 2010-2026; measure proportion of core crime prosecutions handled outside ICC; survey state party declarations on whether domestic courts are becoming viable alternative venues; analyze whether ICC caseload and conviction rate trends suggest entrenchment or maturation-toward-sunset',
    'If capacity distributes: ICC''s universal jurisdiction transitions from monopoly to first-among-equals, and the constraint may shift toward scaffold (sunset clock active). If capacity remains concentrated in ICC: universalist reading entrench further into snare territory (non-party states have no viable alternatives). If regional/national prosecution is ineffective or biased: universalist reading is validated, but tangled_rope classification holds because coordination benefits remain real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_distribution, empirical, 'Trajectory of alternative prosecution mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_univ_tr_t0, rome_statute_jurisdiction__universalist_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rome_univ_tr_t7, rome_statute_jurisdiction__universalist_reading, theater_ratio, 7, 0.5).
narrative_ontology:measurement(rome_univ_tr_t14, rome_statute_jurisdiction__universalist_reading, theater_ratio, 14, 0.48).

% Extraction over time
narrative_ontology:measurement(rome_univ_be_t0, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rome_univ_be_t7, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(rome_univ_be_t14, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 14, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rome_univ_su_t0, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(rome_univ_su_t7, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 7, 0.6).
narrative_ontology:measurement(rome_univ_su_t14, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 14, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, international_criminal_court_arrest_warrant_enforcement).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, unsc_chapter_vii_authority_limitation).

% DUAL FORMULATION NOTE:
% The rome_statute_jurisdiction kernel decomposes into three structurally distinct constraints based on competing interpretations of the Rome Statute's authority-grounding. The universalist_reading assumes maximum ICC reach (universal jurisdiction transcending consent); the sovereigntist_reading assumes minimum reach (consent-based only); the hybrid_complementarity_reading assumes conditional reach (only where domestic courts fail). These readings share the same kernel text but yield different ε values and beneficiary/victim structures. Network links show how the universalist reading constrains and influences the other readings by pre-committing to a particular interpretation of Rome Statute authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
