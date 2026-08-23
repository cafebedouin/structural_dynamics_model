% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Usul al-Fiqh: Hadith Authentication as Prerequisite to Legal Derivation
 *   domain: religious_legal_theory/islamic_jurisprudence
 *
 * SUMMARY:
 *   The Shafi'i reading of usul al-fiqh systematizes Islamic legal
 *   methodology into a strict source hierarchy: Quran, then authenticated
 *   hadith (sahih), then consensus of the Companions (ijma), then analogical
 *   reasoning (qiyas) permitted only when the prior sources are silent. This
 *   reading instantiates a constraint where hadith authentication becomes the
 *   gatekeeping prerequisite for legal derivation. The constraint coordinates
 *   by resolving source conflicts through a clear decision procedure, but
 *   extracts by concentrating epistemic authority in hadith transmission
 *   specialists and madhhab authorities, while subordinating rationalist
 *   jurists who rely on expansive qiyas, istihsan, or independent ra'y. The
 *   claimed type is tangled_rope — genuine coordination function (legal
 *   certainty, methodological clarity) coexists with asymmetric extraction
 *   (gatekeeping authority transferred to hadith specialists).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.58).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.52).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Usul al-Fiqh: Hadith Authentication as Prerequisite to Legal Derivation").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "religious_legal_theory/islamic_jurisprudence").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, 'f6ecefac-5fdb-4851-ac2c-4c86598be612').
narrative_ontology:cs_kernel_codification('f6ecefac-5fdb-4851-ac2c-4c86598be612', formalized).
narrative_ontology:cs_authority_grounding('f6ecefac-5fdb-4851-ac2c-4c86598be612', lineage).
narrative_ontology:cs_interpretation_layer_present('f6ecefac-5fdb-4851-ac2c-4c86598be612').
narrative_ontology:cs_reading_relation('f6ecefac-5fdb-4851-ac2c-4c86598be612', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6ecefac-5fdb-4851-ac2c-4c86598be612', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6ecefac-5fdb-4851-ac2c-4c86598be612', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('f6ecefac-5fdb-4851-ac2c-4c86598be612', foundational, hadith_authentication_prerequisite_to_derivation).
narrative_ontology:cs_axiom_status(hadith_authentication_prerequisite_to_derivation, holdable).
narrative_ontology:cs_axiom_grounding('f6ecefac-5fdb-4851-ac2c-4c86598be612', hadith_authentication_prerequisite_to_derivation, conventional).
narrative_ontology:cs_axiom('f6ecefac-5fdb-4851-ac2c-4c86598be612', foundational, qiyas_permitted_only_when_authenticated_hadith_absent).
narrative_ontology:cs_axiom_status(qiyas_permitted_only_when_authenticated_hadith_absent, holdable).
narrative_ontology:cs_axiom_grounding('f6ecefac-5fdb-4851-ac2c-4c86598be612', qiyas_permitted_only_when_authenticated_hadith_absent, conventional).
narrative_ontology:cs_axiom('f6ecefac-5fdb-4851-ac2c-4c86598be612', foundational, ijma_restricted_to_companions_consensus).
narrative_ontology:cs_axiom_status(ijma_restricted_to_companions_consensus, holdable).
narrative_ontology:cs_axiom_grounding('f6ecefac-5fdb-4851-ac2c-4c86598be612', ijma_restricted_to_companions_consensus, conventional).
narrative_ontology:cs_reference_frame('f6ecefac-5fdb-4851-ac2c-4c86598be612', classical_shafii_usul).
narrative_ontology:cs_drift_state('f6ecefac-5fdb-4851-ac2c-4c86598be612', contemporary_madhhab_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6ecefac-5fdb-4851-ac2c-4c86598be612', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_madhhab_authorities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, independent_mujtahids).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, lay_muslims_seeking_legal_clarity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, hadith_authentication_prerequisite).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, qiyas_subordinate_to_authenticated_hadith).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, ijma_restricted_to_companions_consensus).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, systematized_usul_as_meta_discipline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authentication of hadith chains (isnad) and textual criticism (matn), which becomes the gateway to legal derivation under the Shafi'i hierarchy. Their specialization in hadith sciences (ulum al-hadith) grants them epistemic authority over what counts as binding evidence. They benefit from the structural requirement that no legal ruling can proceed without authenticated hadith when Quran is silent.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary,
    organized, generational, constrained, global).

% Administer and enforce the Shafi'i methodological framework through madrasa curricula, judicial appointments, and fatwa institutions. They set the agenda for what constitutes valid usul, police the boundaries of acceptable qiyas, and define the criteria for hadith authentication. Their institutional position is fused with the school's identity — exit would mean abandoning the tradition that constitutes their authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_madhhab_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, shafii_madhhab_authorities, beneficiary).

% Jurists who derive authority from reasoned opinion (ra'y), expansive qiyas, istihsan, or maslaha mursala. Under the Shafi'i hierarchy, their methodological tools are subordinated: qiyas is permitted only when authenticated hadith is entirely absent, istihsan is rejected, and maslaha mursala has no independent standing. They bear the cost of having their interpretive space compressed and their authority claims marginalized within Shafi'i-dominated contexts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, global).

% Scholars claiming independent ijtihad authority outside madhhab affiliation. The systematized usul al-fiqh as a meta-discipline creates a boundary: to be recognized as a mujtahid, one must demonstrate mastery of the very usul framework that subordinates rationalist methods. Their exit option is to operate outside the madhhab system entirely, losing institutional recognition and judicial eligibility.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, independent_mujtahids, payer,
    moderate, biographical, constrained, global).

% Receive the coordination benefit of a clear, hierarchical legal methodology that reduces uncertainty about which sources bind. They gain predictable access to rulings through a defined chain: Quran → authenticated hadith → Companions' ijma → qiyas. However, they have no meaningful exit — they are bound to whatever school dominates their region, and the constraint's operation is opaque to them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, lay_muslims_seeking_legal_clarity, beneficiary,
    powerless, biographical, trapped, global).

% Proponents of the Hanafi reading where qiyas is expansively applicable, ra'y supplements analogy, and istihsan permits departure for public interest. They would object to the Shafi'i restriction of qiyas and rejection of istihsan, but are structurally excluded from the Shafi'i methodological conversation — each school's usul is internally coherent and does not admit the other's premises as valid within its own framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hanafi_jurists, excluded,
    organized, generational, identity_locked, global).

% Proponents of the Maliki reading where Medinan practice ('amal ahl al-Madina) carries independent evidentiary weight, maslaha mursala is valid, and custom ('urf) is integrated. They would object to the Shafi'i dismissal of Medinan practice as a standalone source and the restriction of ijma to Companions only, but are excluded by the same school-boundary logic.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, maliki_jurists, excluded,
    organized, generational, identity_locked, global).

% Proponents of the Hanbali reading where textual sources are maximally restrictive, qiyas is minimized, and weak hadith is preferred over qiyas. They share the Shafi'i emphasis on hadith but diverge on the role of qiyas and the treatment of weak hadith. They are excluded from the Shafi'i conversation but the structural distance is smaller than with Hanafi or Maliki.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hanbali_jurists, excluded,
    organized, generational, identity_locked, global).

% Analyzes the usul al-fiqh kernel and its four readings as a structural case study in how legal methodologies allocate interpretive authority. Sees the coordination function (resolving source conflicts) and the extraction function (concentrating gatekeeping in hadith specialists) without being subject to any school's authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_legal_theorist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate hierarchy of legal sources that resolves conflicts between Quran, hadith, consensus, and analogy — eliminating the indeterminacy of competing rationalist methods and giving judges a clear decision procedure.
% TRANSFER_FUNCTION: Moves interpretive authority from jurists exercising independent reasoning (ra'y, expansive qiyas, istihsan) to hadith transmission specialists who control authentication, and to madhhab authorities who police the boundaries of valid qiyas and ijma.
% ABSENT_VOICES: Rationalist jurists within the Shafi'i orbit who might have developed a more expansive qiyas; marginalized legal thinkers (e.g., early Zahiri or Mu'tazili-influenced jurists) who rejected hadith authentication as a prerequisite; women scholars historically excluded from hadith transmission networks and madhhab authority structures.
% DISAPPEARANCE_RATIONALE: If the Shafi'i usul hierarchy vanished overnight, Shafi'i courts and madrasas would lose their decision procedure for source conflicts. Judges would face indeterminacy between competing hadiths, between hadith and qiyas, and between competing ijma claims. The madhhab would either fracture into methodological factions or adopt a replacement hierarchy (likely Hanafi or Hanbali usul), reorganizing the entire Shafi'i legal ecosystem.
% FOUNDING_PROBLEM: Early Islamic legal practice suffered from chaotic source pluralism: rival hadiths with contradictory chains, uncontrolled qiyas generating unpredictable rulings, competing consensus claims beyond the Companions, and no meta-framework to adjudicate between them.
% FOUNDING_PROBLEM_CORROBORATION: Al-Shafi'i's own Risala attests the founding problem from the beneficiary side. Hanafi and Maliki jurists (Ibn al-Hajib, al-Qarafi) corroborate from outside the beneficiary set that the problem was real but their schools solved it differently. Modern legal historians (Hallaq, Weiss, Melchert) corroborate the historical reality of source chaos but debate whether the Shafi'i solution was necessary or one of several viable systematizations.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the measurable transfer of interpretive authority from rationalist methods to hadith authentication gatekeepers. The constraint extracts by making hadith authentication a prerequisite — whoever controls isnad criticism and matn evaluation controls the gateway to law. Suppression (0.52) is moderate: the methodology actively excludes rival methods (istihsan, maslaha mursala, expansive qiyas) but does not physically coerce; exclusion is epistemic and institutional. Theater ratio (0.28) is low-moderate: the coordination function (clear hierarchy) is genuine, but a growing share of usul literature performs methodological purity while actual fatwa practice accommodates necessities. Accessibility collapse (0.45) is moderate: alternatives exist (other madhhabs) but are costly to access (identity_locked exit). Resistance (0.55) is moderate: rival schools persist and internal critics exist, but the Shafi'i framework dominates in its regions.
 *
 * PERSPECTIVAL GAP:
 *   From the hadith specialist's seat, the constraint is a rope — it coordinates the chaotic hadith corpus into a reliable legal source. From the rationalist jurist's seat, it is a snare — it suppresses valid reasoning methods to protect hadith scholars' gatekeeping. From the madhhab authority's seat, it is a scaffold that became permanent — the founding problem (source chaos) was real, but the solution hardened into a structure that now extracts from internal dissenters. The engine computes this seat divergence from the structural data; the claimed type (tangled_rope) captures the coordination/extraction hybrid from the authoring seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists are structural beneficiaries (d near 0.0): they control the authentication gateway the constraint makes mandatory. Shafi'i madhhab authorities are agenda_setters with beneficiary capture (d ~0.15): they administer the system and benefit from its institutionalization. Rationalist jurists and independent mujtahids are payers (d near 0.85): their methodological tools are structurally subordinated. Lay Muslims are trapped beneficiaries (d ~0.3): they gain coordination but bear diffuse costs of rigidification. Other schools' jurists are excluded (d undefined): they operate in parallel frameworks. The analytical observer sits at d=0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (chaotic source pluralism) was real and live in the 2nd/8th century. The Shafi'i systematization solved it for its time. But the mandate has atrophied: modern legal systems face different problems (codification, human rights, positive law integration), yet the classical usul hierarchy persists as the gatekeeping structure for religious authority. The constraint now persists by institutional inertia (madrasa curricula, judicial tradition) rather than because the founding problem remains live in its original form. This is mandatrophy — the mandate outlived its function, but the constraint remains because dismantling it would redistribute authority away from current gatekeepers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Are the four madhhab usul readings structurally distinct constraints with different ε values, or are they observational perspectives on a single constraint with variable measurement?',
    'Test ε-invariance: if measuring ''usul al-fiqh'' via Hanafi qiyas-permissiveness yields low extractiveness but measuring via Shafi''i hadith-gatekeeping yields high extractiveness, they are distinct constraints. The ε-invariance principle demands decomposition — which this story performs.',
    'If they are one constraint, the framework''s ε-invariance principle is violated. If distinct, each reading gets its own story, its own stakeholders, its own classification — as authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel decomposes into ε-invariant constraint stories or collapses into measurement variance.').

omega_variable(
    hadith_specialist_material_benefit,
    'Do hadith transmission specialists extract material rents (judicial appointments, stipends, endowment control) or only epistemic authority (interpretive gatekeeping)?',
    'Historical analysis of waqf endowments, judicial appointment records, and stipend registers in Shafi''i-dominated regions (Mamluk Egypt, Ottoman Greater Syria, Southeast Asia) to trace material flows to hadith scholars vs. rationalist jurists.',
    'If material rents flow to hadith specialists, extraction is concrete and the constraint leans toward snare. If only epistemic authority, extraction is structural but the coordination function may be stronger — the classification sits at the tangled_rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_specialist_material_benefit, empirical, 'Whether the beneficiary extraction is material or purely epistemic.').

omega_variable(
    qiyas_restriction_mechanism,
    'Is the restriction of qiyas to ''authenticated hadith absent'' enforced structurally (institutional gatekeeping) or internalized (jurists self-censor because the methodology defines legitimacy)?',
    'Compare fatwa output in Shafi''i courts vs. Shafi''i-influenced modern courts: if jurists invoke qiyas freely when hadith exists but frame it as ''clarification,'' suppression is internalized. If institutional review rejects such fatwas, suppression is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase the omega-adjusted classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_restriction_mechanism, empirical, 'Structural vs. internalized suppression mechanism for the qiyas restriction.').

omega_variable(
    founding_problem_persistence,
    'Does the founding problem (source chaos) persist in contemporary Shafi''i contexts, or has the constraint''s coordination function been fully absorbed into modern legal codes?',
    'Analyze modern family law codes in Shafi''i-majority jurisdictions (Indonesia, Malaysia, Yemen, East Africa): if codes reference classical usul hierarchy for gap-filling, the problem persists. If codes are self-contained and usul is ceremonial, the founding problem is dead and the constraint is piton.',
    'If founding_problem_status resolves to ''dead'' while disappearance_verdict is ''world_rearranges'', the mandatrophy flag triggers — the constraint persists as zombie coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the classical usul hierarchy still performs coordination in modern legal systems or survives only as institutional theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_shafii_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(usul_shafii_tr_t2, usul_al_fiqh_method__shafii_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement(usul_shafii_tr_t4, usul_al_fiqh_method__shafii_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(usul_shafii_tr_t6, usul_al_fiqh_method__shafii_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(usul_shafii_tr_t8, usul_al_fiqh_method__shafii_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(usul_shafii_tr_t10, usul_al_fiqh_method__shafii_reading, theater_ratio, 10, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_shafii_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usul_shafii_be_t2, usul_al_fiqh_method__shafii_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(usul_shafii_be_t4, usul_al_fiqh_method__shafii_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(usul_shafii_be_t6, usul_al_fiqh_method__shafii_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(usul_shafii_be_t8, usul_al_fiqh_method__shafii_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(usul_shafii_be_t10, usul_al_fiqh_method__shafii_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(usul_shafii_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(usul_shafii_su_t2, usul_al_fiqh_method__shafii_reading, suppression_requirement, 2, 0.42).
narrative_ontology:measurement(usul_shafii_su_t4, usul_al_fiqh_method__shafii_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(usul_shafii_su_t6, usul_al_fiqh_method__shafii_reading, suppression_requirement, 6, 0.49).
narrative_ontology:measurement(usul_shafii_su_t8, usul_al_fiqh_method__shafii_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(usul_shafii_su_t10, usul_al_fiqh_method__shafii_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__shafii_reading, 0.1).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four in the usul_al_fiqh_method constraint family. Each reading instantiates a distinct constraint with its own ε, stakeholders, and classification. The Shafi'i reading has higher extractiveness (0.58) than the Maliki reading (expected ~0.45, maslaha mursala provides pressure relief) and the Hanafi reading (expected ~0.40, expansive qiyas distributes authority), but lower than the Hanbali reading (expected ~0.65, maximal textual restriction concentrates authority in textual scholars). The family shares the kernel 'legal methodology from revealed sources' but diverges on the coordination/extraction balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, organized, 0.15).
constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, moderate, 0.85).
constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, powerless, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
