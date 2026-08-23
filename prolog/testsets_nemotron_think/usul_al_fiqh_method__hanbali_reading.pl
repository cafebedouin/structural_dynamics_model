% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Usul al-Fiqh: Maximal Textual Restrictiveness
 *   domain: religious/legal/theoretical
 *
 * SUMMARY:
 *   The Hanbali reading of usul al-fiqh (legal methodology) imposes the
 *   highest textual restrictiveness among the four Sunni schools: Quran and
 *   authenticated hadith are the near-exclusive sources; qiyas (analogical
 *   reasoning) is permitted only in cases of clear textual silence; weak
 *   hadith is preferred over qiyas; and sadd al-dhara'i (blocking the means
 *   to forbidden ends) is deployed aggressively to prevent innovations that
 *   might circumvent textual commands. This reading claims to preserve divine
 *   textual fidelity against human distortion. Structurally, it coordinates a
 *   shared hermeneutic boundary for the Hanbali school while extracting
 *   interpretive authority from rationalist jurists, customary practitioners,
 *   and legal reformers whose methodologies are excluded as bid'a. The
 *   constraint requires active enforcement through scholarly consensus (ijma'
 *   of the school), judicial appointment power, and educational transmission.
 *   Over the classical millennium (roughly 800–1800 CE), extractiveness rose
 *   as the school consolidated institutional control in Najd and later Saudi
 *   Arabia, theater ratio increased as textual fidelity became a performative
 *   identity marker, and suppression requirement hardened as competing
 *   schools were marginalized in Hanbali-dominant territories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.75).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.8).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Usul al-Fiqh: Maximal Textual Restrictiveness").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious/legal/theoretical").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '7ad48a4e-2b83-4db1-81cd-41040db099d1').
narrative_ontology:cs_kernel_codification('7ad48a4e-2b83-4db1-81cd-41040db099d1', fixed_text).
narrative_ontology:cs_authority_grounding('7ad48a4e-2b83-4db1-81cd-41040db099d1', lineage).
narrative_ontology:cs_interpretation_layer_present('7ad48a4e-2b83-4db1-81cd-41040db099d1').
narrative_ontology:cs_reading_relation('7ad48a4e-2b83-4db1-81cd-41040db099d1', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ad48a4e-2b83-4db1-81cd-41040db099d1', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ad48a4e-2b83-4db1-81cd-41040db099d1', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('7ad48a4e-2b83-4db1-81cd-41040db099d1', foundational, textual_sources_maximally_restrictive).
narrative_ontology:cs_axiom_status(textual_sources_maximally_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('7ad48a4e-2b83-4db1-81cd-41040db099d1', textual_sources_maximally_restrictive, deontological).
narrative_ontology:cs_axiom('7ad48a4e-2b83-4db1-81cd-41040db099d1', foundational, weak_hadith_over_qiyas).
narrative_ontology:cs_axiom_status(weak_hadith_over_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('7ad48a4e-2b83-4db1-81cd-41040db099d1', weak_hadith_over_qiyas, deontological).
narrative_ontology:cs_axiom('7ad48a4e-2b83-4db1-81cd-41040db099d1', secondary, sadd_al_dhara_i_preserves_fidelity).
narrative_ontology:cs_axiom_status(sadd_al_dhara_i_preserves_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('7ad48a4e-2b83-4db1-81cd-41040db099d1', sadd_al_dhara_i_preserves_fidelity, deontological).
narrative_ontology:cs_reference_frame('7ad48a4e-2b83-4db1-81cd-41040db099d1', prophetic_revelation_textual_fidelity).
narrative_ontology:cs_drift_state('7ad48a4e-2b83-4db1-81cd-41040db099d1', classical_consolidation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7ad48a4e-2b83-4db1-81cd-41040db099d1', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_ulama).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, legal_development_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, lay_muslims).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, bid_a_rejection_principle).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, sadd_al_dhara_i_validity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, weak_hadith_superiority_over_qiyas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the methodological boundaries of Hanbali legal derivation. Their professional identity, scholarly authority, and institutional positions (judgeships, teaching posts, fatwa offices) are fused with the textualist method. Exit requires abandoning the school identity that constitutes their career and epistemic community.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_scholars, agenda_setter,
    institutional, generational, identity_locked, universal).

% Practice legal reasoning using qiyas, istihsan, maslaha, and ra'y. Within Hanbali-dominated jurisdictions their methodologies are excluded from official fatwa bodies and judicial appointments. They can migrate to Hanafi or Shafi'i environments but lose their intellectual community and must adopt a different usul framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    organized, biographical, constrained, universal).

% Rely on 'urf (custom) and local practice for dispute resolution in tribal, rural, or mercantile contexts. The Hanbali textualist method treats their customs as bid'a unless explicitly validated by text. They lack formal legal education to engage the textualist framework and have no alternative institutional venue.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners, payer,
    moderate, biographical, trapped, regional).

% Receive clear, textually-anchored rulings that claim divine authority and resist human manipulation. They also bear the cost of rigidity: rulings may not fit novel social circumstances (medical ethics, finance, technology) and fatwa shopping is discouraged. Their exit means seeking rulings from other schools, which requires literacy to navigate.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, lay_muslims, beneficiary,
    powerless, biographical, constrained, universal).

% Appoint judges and authorize fatwa bodies. In Hanbali-dominant states (historically Najd, modern Saudi Arabia) they enforce the textualist method as official law. They can shift patronage to other schools for political reasons (Ottoman codification favored Hanafi; modern states mix schools), giving them arbitrage-grade exit across methodological frameworks.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, state_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Hanafi, Maliki, and Shafi'i scholars who maintain competing usul frameworks. They engage Hanbali method in polemic, comparative fiqh, and intra-Sunni dialogue but do not bear its costs nor collect its rents. Their analytical seat sees the full structural landscape of methodological contestation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, other_school_scholars, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual fidelity of divine revelation (Quran and authenticated sunnah) against human innovation in legal derivation, providing a stable, bounded hermeneutic that claims to minimize subjective judicial discretion.
% TRANSFER_FUNCTION: Moves interpretive authority from rationalist analogical reasoning (qiyas), juristic preference (istihsan), public interest (maslaha mursala), and custom ('urf) to textualist scholars who monopolize the determination of what counts as authenticated text and what constitutes bid'a. Extraction flows from suppressed legal development to textualist epistemic control.
% ABSENT_VOICES: Rationalist jurists (Mu'tazila-influenced and philosophical fiqh traditions), customary law practitioners in non-Arab regions (West Africa, Southeast Asia, Central Asia), reformist scholars seeking maslaha-based development for modern contexts (medical ethics, Islamic finance, constitutional law) — excluded by the textualist epistemic closure that treats their methodologies as bid'a rather than legitimate derivation.
% DISAPPEARANCE_RATIONALE: If the Hanbali textualist restrictiveness vanished overnight, qiyas would expand to novel cases, weak hadith would lose preferential status over reasoned analogy, maslaha and 'urf would be admitted as independent sources, sadd al-dhara'i blocking would cease, and legal rulings would diversify dramatically across regions, eras, and social contexts — the entire Hanbali legal edifice would restructure toward the expansive methodologies of the other three schools.
% FOUNDING_PROBLEM: How to derive binding law from revelation without importing human innovation (bid'a) that distorts divine intent, given that early Islamic legal practice showed rampant unauthorized reasoning, fabricated hadith, and customary accretions that threatened the textual basis of the shari'a.
% FOUNDING_PROBLEM_CORROBORATION: Early Hanbali texts (Ahmad ibn Hanbal's correspondence, Ibn Taymiyya's al-Risala al-Kubra, Ibn Qayyim's I'lam al-Muwaqqi'in) attest the founding problem as live and ongoing. Rationalist critics (Ibn Rushd's Bidayat al-Mujtahid, al-Shatibi's al-Muwafaqat, modern reformists like Muhammad Abduh and Fazlur Rahman) attest the problem was polemically constructed to delegitimate rival methodologies. No neutral third-party corroboration exists — the dispute is constitutive of the usul al-fiqh field itself.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint transfers interpretive monopoly from multiple reasoning modalities to a single textualist gatekeeping class. Suppression (0.8) is higher still because persistence depends on actively excluding qiyas, istihsan, maslaha mursala, 'urf, and philosophical reasoning — not merely on participant preference. Theater ratio (0.4) is moderate: the textualist hermeneutic performs genuine coordination (boundary maintenance against bid'a) but a growing share of enforcement activity defends the school's institutional monopoly rather than textual fidelity per se. Accessibility collapse (0.75) is high because once the textualist premise is accepted, alternative derivation paths appear as bid'a rather than legitimate difference. Resistance (0.5) is moderate: other schools persist and critique, but within the Hanbali framework dissent is structurally excluded.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanbali scholar seat, the constraint is genuine coordination — a hermeneutic fence protecting revelation from human corruption. From the rationalist jurist seat, it is enforced epistemic closure — a monopoly that declares their reasoning bid'a. From the customary practitioner seat, it is alien imposition — a textualist framework that cannot accommodate their lived practice. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analyst seat seeing both coordination and extraction simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali scholars (agenda_setter, institutional, identity_locked) sit at the beneficiary end (d ~ 0.15): they collect interpretive authority, institutional positions, and epistemic monopoly. State judiciary (agenda_setter, institutional, arbitrage) sits near symmetric (d ~ 0.45): they enforce the method but can switch schools for political utility. Rationalist jurists (payer, organized, constrained) and customary practitioners (payer, moderate, trapped) sit at the target end (d ~ 0.85 and ~0.9): they bear the cost of excluded methodologies with limited exit. Lay Muslims (beneficiary, powerless, constrained) sit near symmetric (d ~ 0.5): they gain textual clarity but lose adaptive flexibility. Other-school scholars (observer, institutional, analytical) sit at the analytical end (d ~ 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (guarding revelation against bid'a) remains contested: textualists argue the threat of innovation is perpetual; rationalists argue the problem was specific to early hadith fabrication and has been solved by hadith science. The arrangement persists not because the founding problem is live, but because the Hanbali scholarly class has identity-fused with the method — their professional existence requires the boundary. This is mandatrophy: the mandate (textual fidelity) has outlived its specific historical occasion (hadith fabrication crisis), but the constraint persists through identity-locked inertia and institutional capture. The engine should detect this via founding_problem_status=contested + disappearance_verdict=world_rearranges mismatch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'Does this constraint represent a genuine methodological commitment to textual fidelity, or an institutional strategy to monopolize interpretive authority against rival schools?',
    'Comparative analysis of Hanbali fatwa output vs. other schools on identical novel cases (medical ethics, finance, technology): if Hanbali rulings consistently restrict where text is silent while other schools develop reasoned solutions, the extraction interpretation gains support.',
    'If institutional strategy, the constraint is a snare masquerading as coordination; if genuine commitment, it is a tangled_rope with authentic coordination function. Classification shifts from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Commitment frame: this is one reading of the usul_al_fiqh_method kernel (hanbali_reading). Sibling readings: hanafi_reading, maliki_reading, shafii_reading. Disagreement located on scope of qiyas, status of weak hadith, validity of maslaha/''urf, and aggression of sadd al-dhara''i.').

omega_variable(
    textual_restrictiveness_vs_development_suppression,
    'Is the measured suppression of rationalist/customary legal development a necessary cost of textual fidelity, or is textual fidelity the cover story for suppressing development?',
    'Counterfactual: if a Hanbali jurist developed a novel ruling using qiyas on a matter of clear textual silence (permitted by the method''s own rules) and was censured, the suppression exceeds the coordination function.',
    'If suppression exceeds coordination necessity, the constraint''s effective extraction is higher than its coordination function justifies — classification shifts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_restrictiveness_vs_development_suppression, empirical, 'Whether the coordination-extraction boundary in this constraint aligns with its own stated rules or exceeds them.').

omega_variable(
    sadd_al_dhara_i_mechanism,
    'Does sadd al-dhara''i (blocking the means) function as a genuine textual fidelity mechanism or as a general-purpose innovation suppressor?',
    'Catalog sadd al-dhara''i applications in classical Hanbali fiqh: count cases where the blocked means clearly leads to a textual prohibition vs. cases where it blocks a novel practice with no clear textual link.',
    'If predominantly the latter, sadd al-dhara''i is an extraction tool disguised as fidelity — extraction metric should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dhara_i_mechanism, empirical, 'Structural ambiguity in the constraint''s primary enforcement instrument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanbali_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(usul_hanbali_tr_t200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 200, 0.25).
narrative_ontology:measurement(usul_hanbali_tr_t400, usul_al_fiqh_method__hanbali_reading, theater_ratio, 400, 0.3).
narrative_ontology:measurement(usul_hanbali_tr_t600, usul_al_fiqh_method__hanbali_reading, theater_ratio, 600, 0.35).
narrative_ontology:measurement(usul_hanbali_tr_t800, usul_al_fiqh_method__hanbali_reading, theater_ratio, 800, 0.38).
narrative_ontology:measurement(usul_hanbali_tr_t1000, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1000, 0.4).

% Extraction over time
narrative_ontology:measurement(usul_hanbali_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usul_hanbali_be_t200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(usul_hanbali_be_t400, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 400, 0.65).
narrative_ontology:measurement(usul_hanbali_be_t600, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 600, 0.7).
narrative_ontology:measurement(usul_hanbali_be_t800, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 800, 0.73).
narrative_ontology:measurement(usul_hanbali_be_t1000, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1000, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanbali_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(usul_hanbali_su_t200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(usul_hanbali_su_t400, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 400, 0.73).
narrative_ontology:measurement(usul_hanbali_su_t600, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 600, 0.77).
narrative_ontology:measurement(usul_hanbali_su_t800, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 800, 0.79).
narrative_ontology:measurement(usul_hanbali_su_t1000, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1000, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanbali_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hanbali_reading of the usul_al_fiqh_method kernel. The kernel decomposes into four constraint stories (one per school) linked by affects_constraints. Each reading has a distinct ε: Hanbali (0.75, highest restrictiveness), Shafi'i (0.55, moderate restrictiveness with systematized hierarchy), Maliki (0.45, practice-integrated), Hanafi (0.4, most expansive). The upstream constraint (textual revelation as source) influences all four; the Hanbali reading influences downstream fatwa production in Hanbali-dominant jurisdictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, institutional, 0.15).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, organized, 0.85).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, moderate, 0.9).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, powerless, 0.5).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, analytical, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
