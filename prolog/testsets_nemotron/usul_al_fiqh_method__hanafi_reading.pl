% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan
 *   domain: religious/legal
 *
 * SUMMARY:
 *   The Hanafi reading of usul al-fiqh (legal methodology) establishes the
 *   most expansive scope for jurist-driven legal reasoning among the four
 *   Sunni madhhabs. Where the Quran and authenticated Sunnah are silent,
 *   qiyas (analogical reasoning) operates freely; where analogy reaches its
 *   limits, ra'y (reasoned opinion) supplements; where strict analogy
 *   produces hardship or contradicts public interest, istihsan (juristic
 *   preference) permits departure. This triad creates a methodological engine
 *   for continuous legal expansion. The constraint is a tangled rope: it
 *   genuinely coordinates the legal system's adaptation to novel
 *   circumstances (coordination function) while structurally transferring
 *   interpretive authority and professional rents from textual specialists to
 *   rationalist jurists (extraction function), and requires active
 *   enforcement through institutional certification and judicial appointment
 *   to maintain its dominance over rival methodologies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.32).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.28).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "religious/legal").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '52e95e93-68dd-452e-845d-c403caef2eb7').
narrative_ontology:cs_kernel_codification('52e95e93-68dd-452e-845d-c403caef2eb7', formalized).
narrative_ontology:cs_authority_grounding('52e95e93-68dd-452e-845d-c403caef2eb7', lineage).
narrative_ontology:cs_interpretation_layer_present('52e95e93-68dd-452e-845d-c403caef2eb7').
narrative_ontology:cs_reading_relation('52e95e93-68dd-452e-845d-c403caef2eb7', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('52e95e93-68dd-452e-845d-c403caef2eb7', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_reading_relation('52e95e93-68dd-452e-845d-c403caef2eb7', usul_al_fiqh_method__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('52e95e93-68dd-452e-845d-c403caef2eb7', foundational, reason_discovers_divine_wisdom_in_rulings).
narrative_ontology:cs_axiom_status(reason_discovers_divine_wisdom_in_rulings, holdable).
narrative_ontology:cs_axiom_grounding('52e95e93-68dd-452e-845d-c403caef2eb7', reason_discovers_divine_wisdom_in_rulings, deontological).
narrative_ontology:cs_axiom('52e95e93-68dd-452e-845d-c403caef2eb7', foundational, public_interest_justifies_departure_from_analogy).
narrative_ontology:cs_axiom_status(public_interest_justifies_departure_from_analogy, holdable).
narrative_ontology:cs_axiom_grounding('52e95e93-68dd-452e-845d-c403caef2eb7', public_interest_justifies_departure_from_analogy, instrumental).
narrative_ontology:cs_axiom('52e95e93-68dd-452e-845d-c403caef2eb7', secondary, hadith_authentication_not_prerequisite_for_qiyas).
narrative_ontology:cs_axiom_status(hadith_authentication_not_prerequisite_for_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('52e95e93-68dd-452e-845d-c403caef2eb7', hadith_authentication_not_prerequisite_for_qiyas, conventional).
narrative_ontology:cs_reference_frame('52e95e93-68dd-452e-845d-c403caef2eb7', classical_hanafi_usul_framework).
narrative_ontology:cs_drift_state('52e95e93-68dd-452e-845d-c403caef2eb7', post_ottoman_codification_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('52e95e93-68dd-452e-845d-c403caef2eb7', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_madhhab_institutions).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, hadith_specialist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, lay_muslim_litigants).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_muslim_litigants).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, reason_as_independent_legal_source).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, public_interest_as_legislative_principle).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, jurist_discretion_in_silence_of_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jurists trained in rationalist methodology (qiyas, ra'y, istihsan) who gain professional authority, institutional positions, and interpretive control from the expansive scope of analogical reasoning. Their expertise in legal expansion becomes a scarce credential. They can move between madhhabs or into state judicial appointments with relative ease.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_jurists, beneficiary,
    organized, generational, arbitrage, global).

% Madrasas, judicial appointments, and scholarly lineages that administer and reproduce the Hanafi methodological framework. They set curricula, certify jurists, and benefit from the demand for Hanafi legal services across Ottoman, Mughal, and post-colonial jurisdictions. Their institutional survival depends on the method's continued legitimacy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_madhhab_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, hanafi_madhhab_institutions, beneficiary).

% Scholars who prioritize textual fidelity (hadith authentication, Quranic literalism) and see expansive qiyas as illegitimate innovation. They bear the cost of marginalization in Hanafi-dominant jurisdictions where their methodology is treated as rigid or backward. Exit requires abandoning their methodological commitment or relocating to Hanbali/Salafi spheres.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_jurists, payer,
    organized, generational, constrained, global).

% Muhaddithun whose authority rests on isnad mastery and hadith criticism. In the Hanafi reading, their specialized labor is subordinated to rationalist analogy — a hadith may be authenticated but set aside by istihsan. They lose professional standing when rationalist jurists override textual evidence. Exit means accepting subordination or migrating to hadith-centric madhhabs.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hadith_specialist_scholars, payer,
    moderate, biographical, constrained, regional).

% Ordinary Muslims seeking legal rulings on novel issues (commercial contracts, family law, new technologies). They benefit from the method's flexibility in generating solutions where texts are silent. They also bear the cost of unpredictable outcomes when jurist discretion varies. Their exit is limited to forum shopping within available madhhabs.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_muslim_litigants, beneficiary,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, lay_muslim_litigants, payer).

% Ottoman, Mughal, and modern codification bodies that adopted Hanafi law as state law. They observe the method's operational stability and its utility for legislative adaptation. They can commission alternative codifications but rarely overturn the methodological foundation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, state_legal_bureaucracies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic method for deriving legal rulings on novel cases where revelation is silent, using structured analogy (qiyas) supplemented by reasoned opinion (ra'y) and equity correction (istihsan). This coordinates the legal system's expansion across time, geography, and social change without requiring new revelation.
% TRANSFER_FUNCTION: Moves interpretive authority and professional rents from textual specialists (hadith scholars, literalist jurists) to rationalist jurists who control the analogical apparatus. The transfer operates through institutional certification (madrasa degrees, judicial appointments) and the epistemic privilege of defining the 'effective cause' (illa) in qiyas.
% ABSENT_VOICES: Early traditionalist critics (Ahl al-Hadith of 2nd/3rd century) who rejected qiyas entirely as bid'a. Modern Salafi reformers who view istihsan as legislative usurpation. Women and marginalized communities whose interests are mediated through jurist-discretion rather than textual right — they are not in the room when the 'public interest' is defined.
% DISAPPEARANCE_RATIONALE: If the Hanafi methodological framework vanished overnight, the legal systems of the Ottoman successor states, South Asian personal law, and major fiqh academies would lose their operative methodology. New interpretive frameworks would be required — either textualist restriction (Hanbali), practice-based custom (Maliki), or hadith-centric hierarchy (Shafi'i). The global map of Islamic legal authority would restructure.
% FOUNDING_PROBLEM: The Quran and Sunnah provide finite texts; the unfolding of human affairs generates infinite novel cases. A method was needed to extend revelation's guidance to unaddressed situations without claiming new revelation. The Hanafi solution: reason (aql) discovers the divine wisdom (hikma) behind rulings and extends it analogically.
% FOUNDING_PROBLEM_CORROBORATION: Classical Hanafi usul texts (al-Sarakhsi's Usul, al-Bazdawi's Usul) attest the founding problem as live. Shafi'i's Risala and Ibn Hazm's Ihkam attest it as a constructed problem — they argue revelation is sufficient when properly understood (hadith authentication + specific textual evidence). Modern legal historians (Hallaq, Weiss, Melchert) corroborate from outside the beneficiary set: the 'gap' between text and life was itself a methodological construct that rationalist jurists built their authority upon.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).
:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) reflects the measurable professional and institutional rents captured by rationalist jurists relative to textualist alternatives — not maximal but significant across centuries. Suppression (0.28) is moderate: the constraint does not violently silence textualists but structurally subordinates their evidence (hadith) to rationalist analysis (istihsan overriding authenticated hadith). Theater ratio (0.18) is low early (genuine methodological innovation) but rises as the method becomes institutionalized and performative compliance replaces creative juristic reasoning. Accessibility collapse (0.42) is partial: alternative methodologies (Maliki, Shafi'i, Hanbali) remain live and accessible, but within a Hanafi-dominant jurisdiction, the cost of switching is high. Resistance (0.55) is substantial: traditionalist, hadith-centric, and modern reformist critiques persist across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist jurist's seat, the constraint is a rope: it solves the genuine coordination problem of legal expansion with minimal coercion. From the textualist jurist's seat, it is a snare: their specialized evidence (hadith) is structurally excluded by the very architecture of qiyas/istihsan. The engine computes this divergence from the declared beneficiaries/victims and exit structures. The claimed_type 'tangled_rope' reflects the authoring seat's assessment that both coordination and extraction are structurally real and inseparable in this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist jurists and Hanafi institutions are structural beneficiaries (d ~ 0.15-0.25): they collect professional rents, institutional control, and epistemic privilege. The directionality derivation from beneficiary declarations + arbitrage-grade exit (institutional mobility across jurisdictions) places them at the beneficiary end. Textualist jurists and hadith specialists are structural payers (d ~ 0.75-0.85): they bear the cost of epistemic subordination and professional marginalization. Their constrained exit (methodological commitment + geographic lock-in to Hanafi-dominant regions) keeps them near the target end. Lay litigants are near-symmetric (d ~ 0.45): genuine coordination benefit from legal flexibility, diffuse indirect cost from discretionary unpredictability. State bureaucracies are analytical observers (d ~ 0.5): they use the method instrumentally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual finitude vs. case infinity) remains live — novel cases (bioethics, finance, digital rights) still exceed textual coverage. However, the original rationalist solution has accumulated extraction layers: institutional certification rents, professional gatekeeping, and state co-optation. The mandate has not atrophied (the coordination problem persists) but the method has been captured by its beneficiaries. This is not mandatrophy (dead problem, living structure) but capture (live problem, captured solution). The constraint remains a tangled rope, not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_restrictiveness_boundary,
    'Where exactly does the Hanafi reading''s textual restrictiveness end and jurist discretion begin? Is there a stable structural boundary or does the method''s expansiveness recursively redefine ''textual silence''?',
    'Comparative analysis of classical usul texts (Sarakhsi, Bazdawi, Dabusi) tracking how ''silence'' is operationalized across centuries. Does the domain of qiyas expand to absorb cases earlier generations would have deemed textually addressed?',
    'If the boundary is recursive, the constraint''s extractiveness is structurally unbounded — rationalist jurists can always find ''silence'' to expand into. If stable, extraction has a natural ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_restrictiveness_boundary, conceptual, 'Whether the textual-restrictiveness boundary is fixed or recursively expandable.').

omega_variable(
    istihsan_public_interest_definition,
    'Who defines ''public interest'' (maslaha) in istihsan, and does the definition serve the rationalist jurist class''s interests?',
    'Genealogy of istihsan applications: track whether ''public interest'' rulings systematically benefit merchant classes, state authorities, or jurist professional interests vs. marginalized groups. Compare with Maliki maslaha mursala where the definition is more communal.',
    'If ''public interest'' is jurist-defined and jurist-serving, istihsan is an extraction mechanism masquerading as equity. If genuinely communal, it is a coordination mechanism with legitimate distributive function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(istihsan_public_interest_definition, empirical, 'Whether istihsan''s public interest criterion is captured by its administrators.').

omega_variable(
    kernel_framing_alternative,
    'Does the kernel ''usul al-fiqh method'' admit a coherent framing where the four readings are not methodological variants but distinct epistemic commitments about the nature of legal authority?',
    'Reconstruct each reading''s authority_grounding and kernel_codification independently. If they map to different CS patterns (lineage vs. extraction vs. practice vs. expertise), the ''method'' label conflates distinct commitment systems.',
    'If the kernel decomposes into distinct commitment systems, the ε-invariance principle requires separate constraint stories per reading (already done) but also separate CS structures with non-overlapping authority groundings. The current family linkage via affects_constraints may be insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the four madhhab readings represent one kernel with variants or four distinct commitment systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 150, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_tr_t150, usul_al_fiqh_method__hanafi_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_tr_t300, usul_al_fiqh_method__hanafi_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_tr_t450, usul_al_fiqh_method__hanafi_reading, theater_ratio, 450, 0.13).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_tr_t600, usul_al_fiqh_method__hanafi_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_tr_t750, usul_al_fiqh_method__hanafi_reading, theater_ratio, 750, 0.17).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_tr_t900, usul_al_fiqh_method__hanafi_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_tr_t1050, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1050, 0.18).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_tr_t1200, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1200, 0.18).

% Extraction over time
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_be_t150, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 150, 0.15).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_be_t300, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 300, 0.22).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_be_t450, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 450, 0.28).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_be_t600, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 600, 0.3).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_be_t750, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 750, 0.31).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_be_t900, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 900, 0.32).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_be_t1050, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1050, 0.32).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_be_t1200, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1200, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_su_t150, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 150, 0.12).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_su_t300, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 300, 0.18).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_su_t450, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 450, 0.22).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_su_t600, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 600, 0.25).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_su_t750, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 750, 0.27).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_su_t900, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 900, 0.28).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_su_t1050, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1050, 0.28).
narrative_ontology:measurement(usul_al_fiqh_method__hanafi_reading_su_t1200, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1200, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.1).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% The usul_al_fiqh_method kernel decomposes into four constraint stories (hanafi, maliki, shafii, hanbali readings) with distinct ε values and beneficiary/victim structures. This Hanafi reading has the lowest textual restrictiveness (ε=0.32) and benefits rationalist jurists. The Hanbali reading (textual maximalism) would show higher suppression, lower extractiveness, benefiting hadith specialists. The Maliki reading (practice-based) benefits Medinan practice transmitters. The Shafi'i reading (hadith-authentication hierarchy) benefits systematic usul theorists. All four are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanafi_reading, organized, 0.2).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanafi_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
