% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections as Internationally Supervised Guarantor Obligation
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the guarantor reading of the Lausanne
 *   minority-protections kernel: the claim that protections for non-Muslim
 *   minorities in Turkey are internationally supervised obligations,
 *   invocable through guarantor-state diplomacy (chiefly Greece) and European
 *   human rights mechanisms, rather than matters resolved solely by Turkish
 *   domestic courts. This reading is structurally distinct from the expansive
 *   reading (which claims functional continuity of pre-1923 institutional
 *   self-administration, property rights, and clergy formation as substantive
 *   guarantees) and the restrictive reading (which confines Lausanne
 *   protection to individual worship rights and treats institutional
 *   questions as purely domestic). The guarantor reading does not adjudicate
 *   WHAT is protected in substance — it adjudicates WHO gets to adjudicate,
 *   asserting an external supervisory layer over the underlying substantive
 *   dispute. Because it lacks a binding enforcement mechanism (no guarantor
 *   state can compel compliance; ECtHR rulings depend on continued Council of
 *   Europe membership and Committee of Ministers follow-up), its practical
 *   bite is diplomatic leverage and legitimacy pressure rather than direct
 *   constraint on Turkish state behavior — a genuine but weak scaffold, not a
 *   snare on the Turkish state and not a substantive guarantee to minorities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.32).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections as Internationally Supervised Guarantor Obligation").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, 'd98b08e9-f836-4f46-95b3-415a5607c41f').
narrative_ontology:cs_kernel_codification('d98b08e9-f836-4f46-95b3-415a5607c41f', fixed_text).
narrative_ontology:cs_authority_grounding('d98b08e9-f836-4f46-95b3-415a5607c41f', distributed).
narrative_ontology:cs_reading_relation('d98b08e9-f836-4f46-95b3-415a5607c41f', lausanne_minority_protections__restrictive_reading, influences).
narrative_ontology:cs_reading_relation('d98b08e9-f836-4f46-95b3-415a5607c41f', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('d98b08e9-f836-4f46-95b3-415a5607c41f', foundational, adjudicatory_authority_is_externally_shared).
narrative_ontology:cs_axiom_status(adjudicatory_authority_is_externally_shared, holdable).
narrative_ontology:cs_axiom_grounding('d98b08e9-f836-4f46-95b3-415a5607c41f', adjudicatory_authority_is_externally_shared, conventional).
narrative_ontology:cs_axiom('d98b08e9-f836-4f46-95b3-415a5607c41f', secondary, domestic_finality_does_not_extinguish_treaty_standing).
narrative_ontology:cs_axiom_status(domestic_finality_does_not_extinguish_treaty_standing, holdable).
narrative_ontology:cs_axiom_grounding('d98b08e9-f836-4f46-95b3-415a5607c41f', domestic_finality_does_not_extinguish_treaty_standing, conventional).
narrative_ontology:cs_reference_frame('d98b08e9-f836-4f46-95b3-415a5607c41f', guarantor_state_supervisory_standing).
narrative_ontology:cs_drift_state('d98b08e9-f836-4f46-95b3-415a5607c41f', post_council_of_europe_accession_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d98b08e9-f836-4f46-95b3-415a5607c41f', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, greek_orthodox_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, armenian_and_jewish_minorities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states_diplomatic_leverage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, internationalized_minority_protection_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, treaty_supervision_survives_domestic_reinterpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Relies on the guarantor-reading framing to appeal adverse domestic rulings (property confiscation, seminary closure, clergy election interference) to European human rights mechanisms and to invoke guarantor-state diplomatic pressure. Cannot relocate its seat or its historical property base; its leverage exists only insofar as external actors treat Lausanne as internationally supervised rather than a closed domestic matter.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, greek_orthodox_patriarchate, beneficiary,
    moderate, civilizational, trapped, national).

% Smaller in numbers and institutional capacity than the Patriarchate; benefit nominally from the same external-adjudication pathway but have historically invoked it far less, lacking the diplomatic weight of a guarantor-state patron equivalent to Greece's interest in the Patriarchate. Their exit from the domestic legal system is not realistic; their leverage under this reading is largely theoretical.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, armenian_and_jewish_minorities, beneficiary,
    powerless, generational, constrained, national).

% Administers domestic law over minority institutions and asserts that Lausanne matters are settled by domestic courts under Turkish sovereignty. Bears reputational and diplomatic cost when guarantor states or the European Court of Human Rights treat minority disputes as internationally supervised rather than internal affairs; can resist but not unilaterally foreclose the pathway because Turkey remains party to the treaty framework and Council of Europe mechanisms.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, payer,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, turkish_state, agenda_setter).

% Invokes its status as a Lausanne signatory and reciprocal-minority interest (Muslim minority in Western Thrace) to raise Patriarchate treatment in bilateral diplomacy and multilateral fora. Uses the guarantor framing as leverage in broader Greek-Turkish relations; has no enforcement mechanism beyond diplomatic pressure and reciprocity threats, and can disengage from any specific dispute without direct cost to itself.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, greece_as_guarantor_state, agenda_setter,
    institutional, generational, mobile, regional).

% Adjudicates individual and institutional complaints framed under the European Convention rather than Lausanne directly, but its jurisprudence increasingly treats Lausanne-protected status as relevant context. Issues rulings that Turkey is treaty-bound to consider but that carry no automatic domestic enforcement mechanism — compliance depends on Committee of Ministers follow-up and Turkey's continued Council of Europe membership.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_court_of_human_rights, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, european_court_of_human_rights, observer).

% Would prefer the restrictive reading in which Lausanne questions are resolved entirely through domestic statutory interpretation without reference to external supervision; their preferred framing is structurally sidelined whenever the guarantor reading is invoked, since it routes the same disputes outward to bodies whose rulings they do not control.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, domestic_turkish_courts, excluded,
    institutional, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a channel through which minority-protection disputes that would otherwise be resolved unilaterally by domestic courts can be raised with external parties (a guarantor state, the ECtHR, the Council of Europe) who have a treaty-recognized standing interest in the outcome.
% TRANSFER_FUNCTION: Moves diplomatic and reputational leverage from the domestic state to guarantor states and supranational courts on a case-by-case basis; moves little else, since there is no automatic financial transfer or binding enforcement — the resource moved is attention and legitimacy pressure, not material compliance.
% ABSENT_VOICES: Domestic Turkish courts and the segments of Turkish public opinion that regard Lausanne minority questions as fully internal are structurally sidelined by this reading's own premise — their preferred restrictive framing is exactly what the guarantor reading routes around, so they are present in the broader kernel contest but excluded from this reading's own operating logic.
% DISAPPEARANCE_RATIONALE: If the guarantor-reading pathway vanished overnight, the Patriarchate and other minorities would lose their primary external leverage channel and disputes would revert fully to domestic adjudication — a real rearrangement for those actors. But because the pathway has no binding enforcement mechanism today, Turkish domestic practice would likely change little in the short run; the effect is more on future leverage and precedent than on present material outcomes, which is why the verdict is contested rather than settled either way.
% FOUNDING_PROBLEM: At Lausanne in 1923, the framers needed to secure protection for non-Muslim minorities remaining in the new Turkish state as a condition of the population exchange and territorial settlement, and needed some mechanism beyond the new state's own promise to make that protection credible to the minorities and to the states with historic interests in them.
% FOUNDING_PROBLEM_CORROBORATION: Council of Europe monitoring bodies and independent minority-rights researchers (outside both the Turkish state and the Patriarchate) attest that the underlying vulnerability the treaty targeted persists in attenuated form — property disputes and clergy-training restrictions remain live — while noting the international-supervision mechanism itself has weakened compared to the interwar period, when guarantor powers had more direct standing. The Turkish state disputes that any supervision obligation survives beyond individual ECHR complaint mechanisms available to any convention party.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, contested).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because the guarantor reading transfers little of material value directly — its currency is diplomatic attention and reputational exposure, not enforceable remedy. Suppression is moderate (0.32): Turkey does not suppress the pathway outright (it remains an ECHR and Council of Europe member) but does resist and delay engagement with adverse findings. Theater ratio is authored high and rising (0.20 in 1923 to 0.58 in 2025) because as the mechanism has aged, the ratio of diplomatic gesture and rhetorical invocation to actual behavioral change in Turkish domestic practice has grown — guarantor-state statements and ECtHR rulings increasingly function as symbolic markers of a supervisory relationship rather than instruments that reliably move outcomes. Accessibility collapse is moderate (0.40): the pathway to raise a complaint remains open, but the pathway to an enforceable remedy has substantially narrowed since the interwar period when guarantor powers had more direct standing. Resistance is moderately high (0.55) reflecting sustained Turkish state pushback against the internationalized framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Patriarchate and other minorities are declared beneficiaries because the guarantor reading is the doctrinal basis for their principal external leverage; however, their directionality is not symmetric with each other — the Patriarchate has an engaged guarantor-state patron (Greece, via reciprocal Western Thrace interests) while Armenian and Jewish minorities lack an equivalently invested guarantor, so their nominal beneficiary status is thinner in practice. The Turkish state is the payer of reputational and diplomatic cost but retains institutional power and constrained-not-trapped exit (it can decline engagement at real but bounded cost). Greece and the ECtHR are agenda-setters who administer the pathway without bearing its costs directly — their exit options are comparatively mobile/analytical, since they can engage or disengage from any specific dispute without structural consequence to themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification (rather than tangled_rope or snare) reflects that the guarantor reading solves a genuine coordination problem — making minority protection credible to parties who cannot fully trust unilateral domestic self-policing — without requiring the active, victim-bearing enforcement machinery that would make it a tangled rope or snare. Its persistence past any obvious sunset is exactly the theater-ratio-rising pattern: the mechanism was built for a 1920s geopolitical configuration (fresh population exchange, active guarantor-power involvement) and has drifted toward diplomatic ritual as that configuration receded, without ever hardening into either genuine enforcement or formal termination. Because it lacks has_sunset_clause and requires_active_enforcement is false, it sits in the mild, low-extraction end of scaffold — not a coercive constraint on Turkey, but not a resolved success either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guarantor_reading_vs_scope_readings_orthogonality,
    'Is the guarantor reading (who adjudicates) genuinely orthogonal to the expansive/restrictive readings (what is protected), or does asserting external supervision functionally import the expansive reading''s substantive scope by giving minorities a forum sympathetic to broader claims?',
    'Track whether ECtHR and guarantor-state interventions, when they occur, tend to endorse expansive-style substantive claims (property, self-administration, clergy formation) or confine themselves to individual worship rights consistent with the restrictive reading. A pattern of substantive expansion via the guarantor channel would indicate the readings are not independent in practice.',
    'If the guarantor pathway systematically channels expansive-reading outcomes, this story''s low-extractiveness scaffold classification undersells its practical effect; if it stays confined to restrictive-reading-consistent claims, the scaffold-with-low-bite classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_reading_vs_scope_readings_orthogonality, empirical, 'Whether adjudicatory-forum claims and substantive-scope claims are structurally independent in practice.').

omega_variable(
    guarantor_state_asymmetry,
    'Does the guarantor reading function equivalently for all Lausanne-protected minorities, or does it function primarily as a bilateral Greek-Turkish leverage mechanism that incidentally covers Armenian and Jewish minorities without an equivalent guarantor patron?',
    'Compare the frequency and diplomatic weight of interventions on behalf of the Greek Orthodox Patriarchate versus Armenian and Jewish minority institutions over the historical record.',
    'If the mechanism is substantially Greek-Orthodox-specific, the beneficiary declaration for armenian_and_jewish_minorities overstates their actual structural benefit, and a split story may be warranted for those communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_state_asymmetry, empirical, 'Whether guarantor leverage is uniformly available across protected minority groups or concentrated on the Greek Orthodox community via bilateral reciprocity.').

omega_variable(
    enforcement_mechanism_absence_is_definitional_or_contingent,
    'Is the absence of a binding enforcement mechanism intrinsic to the guarantor reading as such, or a contingent historical fact that could change if, e.g., Council of Europe follow-up procedures were strengthened or Greece escalated reciprocity measures?',
    'Examine whether any historical episode has produced material Turkish policy change traceable to guarantor-state or ECtHR pressure, versus episodes where pressure was applied without effect.',
    'If enforcement is contingently weak rather than structurally absent, this constraint could reclassify toward tangled_rope if enforcement capacity is later built; if structurally absent, the scaffold classification with declining real bite (rising theater_ratio) is the stable long-run reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_absence_is_definitional_or_contingent, conceptual, 'Whether the mechanism''s weak enforcement is contingent or structural to the guarantor reading itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 1923, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__guarantor_reading, theater_ratio, 1923, 0.2).
narrative_ontology:measurement(laus_tr_t1945, lausanne_minority_protections__guarantor_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(laus_tr_t1974, lausanne_minority_protections__guarantor_reading, theater_ratio, 1974, 0.42).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__guarantor_reading, theater_ratio, 1990, 0.48).
narrative_ontology:measurement(laus_tr_t2005, lausanne_minority_protections__guarantor_reading, theater_ratio, 2005, 0.53).
narrative_ontology:measurement(laus_tr_t2025, lausanne_minority_protections__guarantor_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1923, 0.12).
narrative_ontology:measurement(laus_be_t1945, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(laus_be_t1974, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1974, 0.2).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(laus_be_t2005, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2005, 0.26).
narrative_ontology:measurement(laus_be_t2025, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(lausanne_minority_protections__guarantor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__guarantor_reading, 0.06).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the lausanne_minority_protections kernel. The restrictive_reading and expansive_reading disagree about WHAT substantive content Lausanne protects (individual worship rights only, versus full institutional continuity including property and clergy formation). This guarantor_reading is a distinct axis — WHO has standing to adjudicate disputes about that substantive content (external supervision versus domestic finality). It is authored as influences rather than forecloses or coexists_with toward both siblings because opening an external adjudicatory channel changes the resource and legitimacy environment in which the substantive dispute is fought, without logically determining which substantive answer prevails and without simply sitting alongside them as an unrelated alternative position. All three stories must be read together to reconstruct the full kernel contest; ε values differ substantially (this reading's extractiveness is authored low at 0.28 given its weak enforcement, while the restrictive and expansive readings carry their own independent ε reflecting the domestic-versus-institutional stakes each substantively claims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
