% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Shafi'i Four-Tier Usul al-Fiqh Hierarchy (Hadith-Transmission Arbitration)
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   This story instantiates the Shafi'i reading of the jurisprudential method
 *   kernel: al-Shafi'i's al-Risala fixes a strict, ranked four-tier hierarchy
 *   of legal sources (Qur'an, Hadith, Ijma, Qiyas) and elevates
 *   hadith-transmission criticism (isnad science) to the arbiter of what
 *   counts as valid textual evidence beneath the Qur'an. This is presented,
 *   and genuinely functions, as a coordination solution to the visible
 *   methodological inconsistency among early schools — but it also
 *   concentrates interpretive authority in the class of scholars who can
 *   adjudicate hadith soundness, and it demotes customary practice and
 *   untethered analogical reasoning from independent-source status to
 *   constrained, lower-tier tools. This is ONE of four sibling readings of
 *   the same underlying kernel (jurisprudential_method_kernel); the Hanafi,
 *   Maliki, and Hanbali readings are separate constraint stories with their
 *   own ε, beneficiaries, and victims — they are not alternative measurements
 *   of this constraint, they are different constraints that happen to share a
 *   contested textual/methodological space.
 *
 * KEY AGENTS:
 *   - hadith_transmission_scholars: Primary beneficiary (institutional/arbitrage) — become indispensable arbiters of source validity
 *   - shafii_school_jurists: Agenda-setter (institutional/arbitrage) — codify, teach, and enforce the hierarchy
 *   - customary_practice_communities: Primary payer (moderate/constrained) — lose independent-source status for 'urf
 *   - independent_analogical_reasoners: Secondary payer (moderate/constrained) — qiyas and istihsan subordinated to fourth tier
 *   - rival_school_jurists: Excluded (organized/constrained) — Hanafi, Maliki, Hanbali methods measured against, not consulted in, the standard
 *   - later_jurisprudence_historians: Analytical observer — traces the formation and contestation of usul al-fiqh
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.58).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.52).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Shafi'i Four-Tier Usul al-Fiqh Hierarchy (Hadith-Transmission Arbitration)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '9287e258-ab3f-474f-84b8-4fecd672cc16').
narrative_ontology:cs_kernel_codification('9287e258-ab3f-474f-84b8-4fecd672cc16', formalized).
narrative_ontology:cs_authority_grounding('9287e258-ab3f-474f-84b8-4fecd672cc16', lineage).
narrative_ontology:cs_interpretation_layer_present('9287e258-ab3f-474f-84b8-4fecd672cc16').
narrative_ontology:cs_reading_relation('9287e258-ab3f-474f-84b8-4fecd672cc16', jurisprudential_method_kernel__hanafi_reading, influences).
narrative_ontology:cs_reading_relation('9287e258-ab3f-474f-84b8-4fecd672cc16', jurisprudential_method_kernel__maliki_reading, influences).
narrative_ontology:cs_reading_relation('9287e258-ab3f-474f-84b8-4fecd672cc16', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('9287e258-ab3f-474f-84b8-4fecd672cc16', foundational, hadith_transmission_is_the_authenticating_arbiter).
narrative_ontology:cs_axiom_status(hadith_transmission_is_the_authenticating_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('9287e258-ab3f-474f-84b8-4fecd672cc16', hadith_transmission_is_the_authenticating_arbiter, conventional).
narrative_ontology:cs_axiom('9287e258-ab3f-474f-84b8-4fecd672cc16', foundational, qiyas_is_subordinate_last_resort_not_independent_source).
narrative_ontology:cs_axiom_status(qiyas_is_subordinate_last_resort_not_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('9287e258-ab3f-474f-84b8-4fecd672cc16', qiyas_is_subordinate_last_resort_not_independent_source, conventional).
narrative_ontology:cs_reference_frame('9287e258-ab3f-474f-84b8-4fecd672cc16', shafii_four_tier_ranked_hierarchy).
narrative_ontology:cs_drift_state('9287e258-ab3f-474f-84b8-4fecd672cc16', post_classical_madhhab_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9287e258-ab3f-474f-84b8-4fecd672cc16', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, shafii_school_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practice_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, independent_analogical_reasoners).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, methodological_standardization_resolves_juristic_inconsistency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Isnad critics and muhaddithun become the load-bearing arbiters of legal validity once hadith is elevated to the second tier and its authentication becomes the gate through which any ruling must pass. Their specialized transmission-chain expertise becomes indispensable capital; disputes among schools are increasingly resolved by appeal to their verdicts on hadith soundness rather than by open juristic reasoning.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars, agenda_setter).

% Al-Shafi'i and his successors codify and enforce the strict four-tier ordering in al-Risala and subsequent usul texts, training qadis and muftis in the method and using it to adjudicate between the looser Hanafi and Maliki approaches. They administer the standard, train its practitioners, and gain authority as the reference method against which rival schools are measured.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, shafii_school_jurists, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Communities whose local custom ('urf) and lived practice had informally shaped legal norms find that customary practice no longer counts as an independent source of law; it must now be re-derived or justified through the four ranked tiers, or discarded if it cannot trace to Qur'an, sound hadith, consensus, or analogy. Their inherited normative practice is devalued unless it can be reframed to satisfy the hierarchy.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practice_communities, payer,
    moderate, generational, constrained, regional).

% Jurists in the earlier, looser tradition of qiyas and istihsan (juristic preference) who exercised relatively free analogical reasoning find their discretion subordinated: qiyas is demoted to the fourth and lowest tier, usable only after Qur'an, hadith, and ijma are exhausted, and istihsan is treated with suspicion as insufficiently disciplined. Their interpretive latitude narrows considerably under the new method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, independent_analogical_reasoners, payer,
    moderate, biographical, constrained, regional).

% Hanafi jurists reliant on extensive qiyas/istihsan, Maliki jurists reliant on Medinan communal practice ('amal ahl al-Madina), and Hanbali jurists reliant on literalism and unanimous consensus are not consulted in the formulation of the Shafi'i hierarchy but are measured against it once it circulates; their own methodological premises are treated as departures needing correction rather than co-equal alternatives.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, rival_school_jurists, excluded,
    organized, generational, constrained, continental).

% Study the formation of usul al-fiqh as a discipline, comparing the four schools' methodological commitments and tracing how al-Shafi'i's systematization became a reference point (sometimes adopted, sometimes contested) across subsequent centuries of legal theory.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, later_jurisprudence_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, ranked, teachable procedure for deriving law that resolves disputes between early, inconsistent regional schools by fixing an order of priority among sources — reducing arbitrary or ad hoc rulings and giving jurists a shared method for adjudicating disagreement.
% TRANSFER_FUNCTION: Moves interpretive authority from diffuse local custom and free analogical reasoning toward a specialized class of hadith critics and the jurists trained in the codified method; moves legitimacy away from customary practice and unstructured qiyas toward transmission-verified textual sources.
% ABSENT_VOICES: Communities whose 'urf had functioned as de facto law, and jurists in the Hanafi/Maliki/Hanbali traditions whose methods are implicitly subordinated by the hierarchy's ranking, are not party to the standardization itself — they inherit a ranking authored primarily by al-Shafi'i and his students and must adapt to it or contest it from outside.
% DISAPPEARANCE_RATIONALE: Proponents within the Shafi'i tradition would say the discipline of usul al-fiqh and much of subsequent Islamic legal methodology depends on this hierarchy — its disappearance would destabilize centuries of doctrinal reasoning built atop it. Critics from rival schools would say law would simply revert to (or persist in) the plural methodological landscape that predated al-Shafi'i, in which Hanafi qiyas, Maliki 'amal, and Hanbali literalism already functioned as live, coherent alternatives — the world would not collapse, it would look like the other three readings of this same kernel.
% FOUNDING_PROBLEM: Early legal schools (proto-Hanafi, proto-Maliki, and others) produced inconsistent rulings because they weighted sources differently and informally — some leaning on regional custom, some on loose analogical reasoning, some on isolated hadith of uncertain reliability — producing visible disagreement without a shared method to adjudicate it.
% FOUNDING_PROBLEM_CORROBORATION: Shafi'i jurists and hadith scholars attest the inconsistency problem was real and that the four-tier hierarchy solved it durably. Historians of Islamic law outside the Shafi'i tradition (including scholars documenting Hanafi, Maliki, and Hanbali critiques) attest that the 'inconsistency' framing itself reflects al-Shafi'i's polemical positioning against the very schools whose methods it displaced — the other schools did not experience their own practice as disordered prior to his intervention, which complicates any claim that the problem was self-evidently in need of this particular solution.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, contested).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored medium-high (0.58) reflecting the real transfer of interpretive authority toward hadith-transmission specialists and away from customary/analogical sources, but not extreme, because the hierarchy retains a genuine and substantial coordination function (resolving inter-school inconsistency) that the more purely extractive end of the scale would not credit. Suppression sits at 0.52 — moderate, because adherence spread through scholarly persuasion, pedagogical institutionalization (madrasas), and juristic consensus-building rather than through state coercion in most times and places, though later state adoption of Shafi'i method in some regions did add enforcement weight. Theater ratio is authored low-to-moderate and rising slowly (0.10 to 0.28) reflecting that the substantive methodological function (actually adjudicating disputes via the hierarchy) remains largely intact and functional over the interval, with only a gradual increase in ceremonial invocation of the hierarchy in later centuries as a marker of school identity rather than active method. Accessibility collapse (0.6) is moderate: once trained in usul al-fiqh, jurists inside the tradition find alternative methodologies increasingly hard to justify on the hierarchy's own terms, but the alternative schools remained genuinely live options throughout Islamic legal history, so collapse is not near-total.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of hadith transmission scholars and Shafi'i jurists, the hierarchy is functioning coordination — it resolved a genuine crisis of methodological inconsistency and gave the discipline of usul al-fiqh its shape. From the seat of customary practice communities and independent analogical reasoners, the same structure operates as an enforced narrowing of what counts as legitimate legal reasoning, requiring active institutional maintenance (teaching, credentialing, adjudication) to keep the ranked hierarchy authoritative against the live alternative methods the sibling readings represent.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission scholars and Shafi'i jurists sit near the beneficiary end: the hierarchy was substantially their methodological innovation, and their specialized skill (isnad criticism) becomes structurally load-bearing under it, with wide institutional mobility (arbitrage exit — they can move between courts, madrasas, and advisory roles carrying transferable authority). Customary practice communities and independent analogical reasoners sit nearer the target end: their prior standing as independent or near-independent sources of legal legitimacy is demoted, and their exit is constrained — they cannot simply opt out of a legal system while continuing to participate in it, but must either reframe their practice to satisfy the tiers or accept diminished authority. Rival school jurists are treated as excluded rather than beneficiary/victim in the strict sense: they are not coordinated by this specific reading (their own readings coordinate them), but they bear the reputational and institutional pressure of being implicitly measured against a rival standard they did not help author.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inconsistent early rulings, absence of a shared adjudicative method) is genuinely contested as to whether it remains live: within Shafi'i and later Sunni legal scholarship broadly, the four-tier hierarchy is treated as a settled, foundational achievement whose continued operation is still necessary (the problem it solved recurs whenever new cases arise). Critics note that this framing is corroborated primarily from within the tradition it establishes — the 'inconsistency' al-Shafi'i diagnosed in Hanafi and Maliki method looks, from outside the Shafi'i lineage, less like disorder and more like a different, internally coherent methodological commitment (extensive qiyas/istihsan for Hanafis, living Medinan practice for Malikis). This is precisely the tangled-rope signature: a real coordination function (a shared method for resolving disputes) bundled with an asymmetric transfer (authority moving toward hadith specialists and away from custom and free analogy) that requires the active work of jurisprudential institutions (madrasas, qadi appointments, fatwa councils) to sustain against the live alternative readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_authentication_objectivity,
    'Is hadith-transmission (isnad) criticism a genuinely objective, verifiable authentication procedure, or does it retain enough interpretive latitude that its practitioners function as a de facto discretionary authority comparable to the qiyas/istihsan they displace?',
    'Comparative study of isnad-critical disagreement rates among hadith scholars on contested transmissions, and analysis of whether authentication verdicts correlate with pre-existing doctrinal commitments of the scholars issuing them.',
    'If isnad criticism is substantially objective, the hierarchy''s elevation of hadith scholars reflects genuine epistemic merit rather than rent extraction; if it retains significant discretionary latitude, the ''arbiter'' role functions similarly to the qiyas discretion it was meant to discipline, undermining the claimed methodological improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authentication_objectivity, empirical, 'Whether hadith authentication is objective verification or disguised discretion.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the Shafi''i reading''s characterization of pre-Shafi''i method as ''inconsistent'' a neutral empirical description, or is it itself a polemical framing internal to the reading being evaluated, such that a Hanafi or Maliki observer would not recognize their own tradition as disordered prior to al-Shafi''i''s intervention?',
    'Comparative textual analysis of Hanafi and Maliki self-descriptions of their own method prior to and independent of al-Shafi''i''s critique, versus al-Shafi''i''s characterization of them in al-Risala and Ikhtilaf al-Hadith.',
    'If the inconsistency framing is polemical rather than neutral, the founding-problem narrative in this story is itself a product of the reading it purports to justify — corroboration from outside the Shafi''i tradition becomes essential and currently thin, strengthening the case for tangled_rope over a cleaner rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the ''inconsistency'' the hierarchy claims to solve is a neutral or a partisan characterization.').

omega_variable(
    beneficiary_capture_over_time,
    'Did the concentration of authority in hadith-transmission scholars deepen into occupational capture (a self-perpetuating credentialing guild) over the centuries following al-Shafi''i, beyond what the coordination function requires?',
    'Historical tracing of hadith-scholar institutional privileges, madrasa curricula weighting, and qadi appointment patterns across the medieval period to detect whether authority concentration outpaced any plausible growth in the underlying coordination need.',
    'Rising capture would support the observed extractiveness trend in the temporal measurements and reinforce classification pressure toward tangled_rope or even snare in later periods; stable or declining capture would support a more benign rope-like reading over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_over_time, empirical, 'Whether hadith-scholar authority concentration exceeded coordination need over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(juri_tr_t0, projected).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__shafii_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement_basis(juri_tr_t200, projected).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__shafii_reading, theater_ratio, 400, 0.18).
narrative_ontology:measurement_basis(juri_tr_t400, projected).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__shafii_reading, theater_ratio, 600, 0.21).
narrative_ontology:measurement_basis(juri_tr_t600, projected).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__shafii_reading, theater_ratio, 900, 0.25).
narrative_ontology:measurement_basis(juri_tr_t900, projected).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__shafii_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(juri_tr_t1200, projected).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(juri_be_t0, projected).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 200, 0.44).
narrative_ontology:measurement_basis(juri_be_t200, projected).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 400, 0.5).
narrative_ontology:measurement_basis(juri_be_t400, projected).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 600, 0.53).
narrative_ontology:measurement_basis(juri_be_t600, projected).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 900, 0.56).
narrative_ontology:measurement_basis(juri_be_t900, projected).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement_basis(juri_be_t1200, projected).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(juri_su_t0, projected).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 200, 0.46).
narrative_ontology:measurement_basis(juri_su_t200, projected).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 400, 0.48).
narrative_ontology:measurement_basis(juri_su_t400, projected).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 600, 0.5).
narrative_ontology:measurement_basis(juri_su_t600, projected).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 900, 0.51).
narrative_ontology:measurement_basis(juri_su_t900, projected).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 1200, 0.52).
narrative_ontology:measurement_basis(juri_su_t1200, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__shafii_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of jurisprudential_method_kernel, each a distinct constraint with its own ε, beneficiaries, and victims rather than a shared measurement of one underlying constraint. shafii_reading (this story) authors medium-high ε concentrated on hadith-transmission authentication and structural subordination of custom/analogy; hanafi_reading is expected to author lower ε with beneficiaries among qiyas-practicing jurists and victims among strict traditionists; maliki_reading is expected to author its ε around the authority of Medinan communal practice with beneficiaries among Medinan jurists and victims among non-Medinan schools whose practice is treated as less authoritative; hanbali_reading is expected to author ε concentrated on the rejection of qiyas/istihsan as illegitimate innovation, with beneficiaries among literalist traditionists and victims among analogical reasoners. All four link to each other via affects_constraints because each reading's institutional success structurally pressures the legitimacy and resource base of the others (shared students, shared textual corpus, competing claims to authentic transmission of the same founding tradition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
