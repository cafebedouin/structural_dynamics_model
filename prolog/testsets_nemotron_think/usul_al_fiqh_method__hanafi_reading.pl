% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   domain: legal/theological
 *
 * SUMMARY:
 *   The Hanafi reading of usul al-fiqh holds that when textual sources
 *   (Quran, authenticated hadith) are silent, analogical reasoning (qiyas) is
 *   expansively applicable; reasoned opinion (ra'y) supplements where analogy
 *   reaches its limits; and juristic preference (istihsan) permits departure
 *   from strict analogy for considerations of public interest (maslaha). This
 *   reading — institutionalized through the Hanafi madhhab, the Ottoman qadi
 *   system, and the Majalla codification — structures legal derivation across
 *   the former Ottoman lands, South Asia, and Central Asia. It is one of four
 *   major readings of the usul al-fiqh kernel, distinguished by the lowest
 *   textual restrictiveness and highest scope for jurist-driven analogical
 *   expansion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.62).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/theological").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, 'b0417f7c-1629-40fa-beba-1d7d8ce5a1ec').
narrative_ontology:cs_kernel_codification('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', distributed).
narrative_ontology:cs_authority_grounding('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', lineage).
narrative_ontology:cs_interpretation_layer_present('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec').
narrative_ontology:cs_reading_relation('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', foundational, expansive_qiyas_when_text_silent).
narrative_ontology:cs_axiom_status(expansive_qiyas_when_text_silent, holdable).
narrative_ontology:cs_axiom_grounding('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', expansive_qiyas_when_text_silent, conventional).
narrative_ontology:cs_axiom('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', foundational, istihsan_for_public_interest).
narrative_ontology:cs_axiom_status(istihsan_for_public_interest, holdable).
narrative_ontology:cs_axiom_grounding('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', istihsan_for_public_interest, conventional).
narrative_ontology:cs_reference_frame('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', classical_hanafi_usul).
narrative_ontology:cs_drift_state('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', post_ottoman_codification, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('b0417f7c-1629-40fa-beba-1d7d8ce5a1ec', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_jurist_class).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, lay_muslims_hanafi_regions).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_muslims_hanafi_regions).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, legal_reasoning_adapts_to_novelty).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, jurist_authority_in_derivation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, transmit, and administer the Hanafi usul method across madrasas and qadi courts. Their interpretive authority derives from chains of isnad back to Abu Hanifa and his students. They set the agenda for what counts as valid qiyas, when istihsan applies, and how ra'y is disciplined. Exit means abandoning the school's cumulative precedent — professionally costly but not impossible.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, constrained, continental).

% Jurists trained in the rationalist disciplines (kalam, logic, usul) who gain hermeneutic authority from the Hanafi method's expansive scope for reasoned analogy. They benefit from the premium placed on intellectual virtuosity in legal derivation. Their exit options include migrating to other rationalist-friendly schools (Shafi'i, Mutazilite-influenced circles) or secular legal academia.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_jurist_class, beneficiary,
    organized, biographical, mobile, global).

% Scholars who hold that legal derivation must remain tightly bound to textual sources (Quran, authenticated hadith, Companion consensus). The Hanafi method's expansive qiyas and istihsan structurally marginalize their approach, treating textual silence as an invitation to reason rather than a boundary. Their identity is fused to textual fidelity — exit would constitute apostasy from their epistemic commitment.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_jurists, payer,
    organized, generational, identity_locked, continental).

% Communities living under Hanafi-dominated legal systems (Ottoman, Mughal, Central Asian). They benefit from legal flexibility that accommodates local custom and changing circumstances. They pay indirectly when juristic discretion produces unpredictable or self-serving rulings. Exit means migration to regions governed by other madhhabs — geographically possible but socially disruptive.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_muslims_hanafi_regions, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, lay_muslims_hanafi_regions, payer).

% The Ottoman state adopted the Hanafi school as its official madhhab, funding madrasas, appointing qadis, and ultimately codifying Hanafi law in the Majalla. They benefit from a legal system that can generate administrative rules for a vast empire. They could switch imperial patronage to another school (as the Safavids did with Twelver Shi'ism) but the sunk cost of Hanafi institutionalization is immense.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, ottoman_authorities, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Modern scholars of Islamic law, comparative law, and legal theory who analyze the Hanafi method from outside the tradition. They neither collect rents nor bear costs from the constraint's operation. Their exit is costless — they can shift research focus without professional penalty.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic, teachable method for deriving binding legal rulings in the vast domain of human activity where revelation is silent, enabling legal continuity across time, geography, and novel circumstances without requiring new revelation.
% TRANSFER_FUNCTION: Moves hermeneutic authority from the fixed textual sources to the trained jurist class, concentrating the power to determine what the law *is* in the hands of those who master the rationalist disciplines of usul, qiyas, and istihsan.
% ABSENT_VOICES: Early traditionalist critics (Ahl al-Hadith) who warned that expansive qiyas opens the door to unrestrained opinion; later Salafi reformers who argue the method institutionalized bid'ah; contemporary textualist movements who see the Hanafi legacy as the primary obstacle to a 'purified' sharia. These voices were structurally excluded from the Hanafi institutional apparatus (madrasas, qadi appointments, state patronage).
% DISAPPEARANCE_RATIONALE: If the Hanafi method vanished overnight, the positive law of the Ottoman, Mughal, and post-Ottoman successor states (codified in the Majalla, personal status codes, and modern civil codes) would lose its doctrinal foundation. The entire edifice of Hanafi fiqh — thousands of furu' rulings derived through its usul — would become unmoored from its justificatory chain. New interpretive authorities would need to be constituted.
% FOUNDING_PROBLEM: How to generate binding, systematic legal rulings for the immense domain of human transactions, family relations, and state administration that the Quran and Sunnah address only in general principles or not at all — without claiming new revelation.
% FOUNDING_PROBLEM_CORROBORATION: Schacht (Origins of Muhammadan Jurisprudence), Coulson (History of Islamic Law), and Hallaq (Authority, Continuity and Change) attest the founding problem from outside the Hanafi tradition as a structural feature of early Islamic legal development. Hanafi usul works (Usul al-Sarakhsi, al-Bazdawi, al-Dabusi) attest it internally. Textualist critics (Ibn Taymiyya, modern Salafi scholars) dispute that the problem exists in the form Hanafis describe, arguing the textual sources are sufficient when properly authenticated and understood.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the concentration of hermeneutic authority in a trained jurist class whose rationalist credentials gatekeep legal validity. Suppression (0.58) captures the structural marginalization of textualist approaches — not through direct coercion but through institutional dominance of madrasa curricula, qadi appointments, and state patronage. Theater ratio (0.38) rises in the Ottoman codification period (Majalla) where the living ijtihad tradition was frozen into statute, creating performative fidelity to a method no longer dynamically practiced. The measurement grid uses centuries (0=8th, 12=20th): extraction rises as the school institutionalizes, peaks with state enforcement, then slightly declines as modern nation-states displace the madhhab system; theater spikes during codification; suppression peaks when the state actively excludes rival madhhabs from official posts.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanafi jurist seat, the method is a Rope: a genuine coordination solution to the problem of legal completeness, minimally coercive, with participants (jurists, lay Muslims, state) as net beneficiaries. From the textualist jurist seat, the same structure is a Snare: the coordination story covers the extraction of hermeneutic authority from textual sources to a rationalist elite, persistence depends on institutional suppression of alternatives, and the victims are identifiable. The engine computes this divergence from the declared beneficiaries, victims, power levels, and exit options — the claimed_type 'tangled_rope' acknowledges the hybrid reality without adjudicating between seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists and the rationalist jurist class are structural beneficiaries (d near 0.1-0.2): they collect interpretive authority, professional prestige, and institutional positions from the method's expansive scope. Textualist jurists are structural targets (d near 0.8-0.9): their competing epistemic commitment is marginalized by the very structure of the method, and their identity_locked exit makes the extraction inescapable from within their framework. Lay Muslims sit near symmetric (d~0.5): genuine coordination benefit (flexible law) and diffuse cost (unpredictability). Ottoman authorities are beneficiaries with arbitrage-grade exit (d~0.15): they capture the method's administrative utility but could theoretically switch patronage. Observers sit at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (generating law for revelation's silence) remains live — novelty in human affairs is open-ended. But the Hanafi method's specific institutional form (madrasa transmission, qadi courts, Majalla codification) has largely atrophied. The method persists as a reference point in modern personal status codes and Islamic finance, but the living ijtihad tradition that justified its extraction has been replaced by statutory freezing. This is mandatrophy: the mandate (solve legal novelty) outlived the specific institutional form, yet the form persists through inertia and identity. The textualist claim to limit innovation is the displaced victim — its suppression is no longer functionally necessary for the coordination function, making the residual extraction harder to justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Hanafi usul a distinct constraint from the kernel usul_al_fiqh_method, or merely a parameterization of a single constraint?',
    'Apply the epsilon-invariance test: if measuring the constraint via the Hanafi reading''s operational criteria (scope of qiyas, availability of istihsan, role of ra''y) yields a different ε than measuring via the Hanbali reading''s criteria (textual restriction, hadith preference, innovation blocking), they are distinct constraints requiring separate stories.',
    'If distinct, each reading gets its own ε, stakeholders, and classification linked by network.affects_constraints. If not, the framework must model observable-dependent ε — which the ε-invariance principle forbids.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to epsilon-invariance across kernel readings').

omega_variable(
    structural_delta_vs_siblings,
    'Does the Hanafi reading''s structural delta (lowest textual restrictiveness, highest jurist-driven expansion) produce a genuinely different beneficiary/victim structure than the sibling readings, or merely a difference in degree?',
    'Compare the beneficiary and victim arrays across all four readings. If Hanafi uniquely benefits ''rationalist_jurist_class'' and victims ''textualist_jurists'' while Maliki benefits ''Medinan_practice_transmitters'' and victims ''hadith_critics'', Shafi''i benefits ''usul_systematizers'' and victims ''particularist_jurists'', Hanbali benefits ''textualist_jurists'' and victims ''rationalist_jurists'' — then the beneficiary/victim structures are distinct, confirming distinct constraints.',
    'Distinct beneficiary/victim structures confirm the epsilon-invariance decomposition. Overlapping structures would suggest a single constraint with a continuous parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_vs_siblings, empirical, 'Whether sibling readings have disjoint beneficiary/victim structures').

omega_variable(
    textualist_claim_as_victim,
    'Is ''textualist claim to limit innovation'' a victim group (agents who bear costs) or a vindicated proposition (a doctrine whose non-vindication is the cost)?',
    'Trace the costs: do individual textualist jurists lose professional positions, credibility, or institutional access because of the Hanafi method''s dominance? Or is the cost purely the non-acceptance of their doctrinal position? The former makes them victims (agents); the latter makes the claim a vindicated proposition.',
    'If agents, they appear in base_properties.victims and stakeholders[] as payer. If proposition, it appears in vindicated_propositions. The classification gates (Tangled Rope requires victims) depend on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_claim_as_victim, conceptual, 'Agent vs. proposition status of the textualist position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanafi_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(usul_hanafi_tr_t3, usul_al_fiqh_method__hanafi_reading, theater_ratio, 3, 0.18).
narrative_ontology:measurement(usul_hanafi_tr_t6, usul_al_fiqh_method__hanafi_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(usul_hanafi_tr_t9, usul_al_fiqh_method__hanafi_reading, theater_ratio, 9, 0.45).
narrative_ontology:measurement(usul_hanafi_tr_t12, usul_al_fiqh_method__hanafi_reading, theater_ratio, 12, 0.38).

% Extraction over time
narrative_ontology:measurement(usul_hanafi_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(usul_hanafi_be_t3, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(usul_hanafi_be_t6, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(usul_hanafi_be_t9, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 9, 0.65).
narrative_ontology:measurement(usul_hanafi_be_t12, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 12, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanafi_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(usul_hanafi_su_t3, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(usul_hanafi_su_t6, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(usul_hanafi_su_t9, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(usul_hanafi_su_t12, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.08).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint and its three siblings form the usul_al_fiqh_method kernel family. Each reading instantiates a distinct constraint with its own ε, stakeholder structure, and classification. The Hanafi reading (this story) has the lowest textual restrictiveness and highest jurist-driven analogical scope. The Hanbali reading has the highest textual restrictiveness. The Maliki and Shafi'i readings occupy intermediate positions with different coordination functions (Medinan practice, hadith authentication). All four are linked bidirectionally in the compiled network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanafi_reading, institutional, 0.15).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanafi_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
