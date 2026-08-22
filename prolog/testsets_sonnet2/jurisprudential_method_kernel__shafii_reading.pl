% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Shafi'i Reading: Strict Four-Tier Legal Hierarchy with Hadith as Methodological Arbiter
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This story instantiates the Shafi'i reading of the jurisprudential method
 *   kernel: the claim that valid Islamic law derives from a strict, ordered
 *   four-tier hierarchy (Qur'an, authenticated Hadith, Ijma, Qiyas), with
 *   al-Shafi'i's Risala standardizing hadith transmission criteria as the
 *   decisive arbiter between competing regional legal cultures. This reading
 *   is generated independently of the sibling readings (Hanafi, Maliki,
 *   Hanbali) per the ε-invariance principle: each reading is a structurally
 *   distinct constraint with its own beneficiary/victim structure and its own
 *   ε, linked only via network edges and the shared kernel_id, not merged
 *   into one story.
 *
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
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Shafi'i Reading: Strict Four-Tier Legal Hierarchy with Hadith as Methodological Arbiter").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '7dc08664-0f95-44df-8ca6-554cb1456e77').
narrative_ontology:cs_kernel_codification('7dc08664-0f95-44df-8ca6-554cb1456e77', formalized).
narrative_ontology:cs_authority_grounding('7dc08664-0f95-44df-8ca6-554cb1456e77', lineage).
narrative_ontology:cs_interpretation_layer_present('7dc08664-0f95-44df-8ca6-554cb1456e77').
narrative_ontology:cs_reading_relation('7dc08664-0f95-44df-8ca6-554cb1456e77', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('7dc08664-0f95-44df-8ca6-554cb1456e77', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('7dc08664-0f95-44df-8ca6-554cb1456e77', jurisprudential_method_kernel__hanbali_reading, influences).
narrative_ontology:cs_axiom('7dc08664-0f95-44df-8ca6-554cb1456e77', foundational, hadith_transmission_is_decisive_arbiter).
narrative_ontology:cs_axiom_status(hadith_transmission_is_decisive_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('7dc08664-0f95-44df-8ca6-554cb1456e77', hadith_transmission_is_decisive_arbiter, conventional).
narrative_ontology:cs_axiom('7dc08664-0f95-44df-8ca6-554cb1456e77', foundational, sources_are_strictly_ordered_not_weighted).
narrative_ontology:cs_axiom_status(sources_are_strictly_ordered_not_weighted, holdable).
narrative_ontology:cs_axiom_grounding('7dc08664-0f95-44df-8ca6-554cb1456e77', sources_are_strictly_ordered_not_weighted, conventional).
narrative_ontology:cs_reference_frame('7dc08664-0f95-44df-8ca6-554cb1456e77', shafii_risala_standardized_hierarchy).
narrative_ontology:cs_drift_state('7dc08664-0f95-44df-8ca6-554cb1456e77', post_classical_taqlid_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7dc08664-0f95-44df-8ca6-554cb1456e77', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, isnad_critics).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, medinan_amal_practitioners).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, independent_qiyas_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, regional_customary_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, sunni_muslim_populace).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, sunni_muslim_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy the newly decisive gatekeeping position: under al-Shafi'i's standardization, whether a ruling is valid now turns substantially on isnad (chain of transmission) authentication, which is their specialized craft. Their expertise becomes indispensable to legal outcomes across every school that adopts the methodology, and their scholarly authority and patronage expand accordingly.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars, agenda_setter).

% Administer and teach the four-tier methodology (usul al-fiqh) as the standardized arbiter of legal validity. Their professional identity, training pipeline, and institutional standing (madrasa chairs, qadi appointments) are constituted by fidelity to this hierarchy; abandoning it would dissolve the school's distinct claim to methodological rigor.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, shafii_school_jurists, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Held that the living, continuously transmitted practice of the Medinan community was itself a reliable and independent witness to the Prophet's normative example, sometimes overriding solitary hadith reports. Under the Shafi'i hierarchy their communal practice is demoted beneath authenticated hadith transmission, stripping their tradition of independent evidentiary weight unless it can be reconstructed as, or corroborated by, formal hadith.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, medinan_amal_practitioners, payer,
    moderate, generational, constrained, regional).

% Practiced expansive analogical reasoning and juristic preference (istihsan) to extend rulings to novel cases, treating reasoned judgment as a robust, near-autonomous source. Under the strict hierarchy their tool is demoted to the fourth and last tier, usable only after Qur'an, hadith, and consensus are exhausted, and disciplined by hadith-derived precedent rather than free reasoning.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, independent_qiyas_jurists, payer,
    moderate, generational, constrained, regional).

% Local judges and community elders whose customary rulings (urf) carried force in their districts before standardization. Once hadith-transmission literacy becomes the arbiter of legitimate legal reasoning, their authority is subordinated to jurists trained in isnad criticism, and their local rulings require validation against a translocal textual apparatus they do not control.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, regional_customary_authorities, payer,
    powerless, generational, trapped, local).

% Gain a more portable, consistent adjudication standard that reduces arbitrary regional variation in rulings on worship, contracts, and family law. They also bear the cost where local, context-sensitive customary or communal judgments they trusted are overridden by translocal hadith-based rulings they cannot easily contest without specialized training.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, sunni_muslim_populace, beneficiary,
    organized, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, sunni_muslim_populace, payer).

% Continue to hold rival methodological commitments (literalist Companion-opinion primacy, expansive qiyas/istihsan, or Medinan amal) but operate within an intellectual field increasingly organized around the Shafi'i hierarchy as the reference standard for usul al-fiqh debate, even when rejecting its conclusions. They are not consulted in constructing this reading's internal logic; they contest it from outside.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, later_hanbali_hanafi_maliki_jurists, excluded,
    organized, civilizational, constrained, continental).

% Study how al-Shafi'i's Risala reorganized competing regional legal cultures into a hierarchy of sources, and assess whether the standardization resolved genuine inconsistency or primarily elevated one professional guild's method as arbiter over others'.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, islamic_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, ordered procedure for deriving law (Qur'an, then authenticated Hadith, then Ijma, then Qiyas) that lets jurists across regions resolve disputes about which source prevails when texts, community practice, and analogical reasoning conflict, reducing arbitrary or purely local variation in rulings.
% TRANSFER_FUNCTION: Moves interpretive authority and the associated social/legal capital from holders of localized, non-textual forms of authority (living community practice, unconstrained analogical reasoning, customary adjudication) to specialists in hadith authentication and the school's methodological apparatus.
% ABSENT_VOICES: Medinan amal practitioners, independent qiyas jurists, and regional customary authorities would object that their forms of transmitted knowledge are demoted to subordinate or merely corroborative status; they are represented in later juristic polemics but were not architects of this hierarchy's construction.
% DISAPPEARANCE_RATIONALE: If the strict four-tier hierarchy vanished as the operative standard, regional schools would revert to more heterogeneous weighting of community practice, juristic discretion, and hadith — reopening disputes the standardization was built to settle, and diminishing the specialized authority currently held by hadith-transmission scholars and Shafi'i-trained jurists.
% FOUNDING_PROBLEM: Early Islamic legal schools disagreed inconsistently about how to weigh Qur'an, hadith reports of varying reliability, communal practice, and analogical reasoning against one another, producing regionally divergent and sometimes contradictory rulings without a shared method for adjudicating between sources.
% FOUNDING_PROBLEM_CORROBORATION: Shafi'i school jurists and hadith-transmission scholars attest the methodological problem remains live, citing continued need for disciplined source-weighting. Historians of Islamic law and jurists from rival schools (Maliki, Hanafi, Hanbali traditions) attest that the underlying diversity of legitimate legal reasoning was never fully a 'problem' requiring resolution by hierarchy — it reflected genuine regional epistemic diversity that the standardization suppressed rather than reconciled, a reading independent of the Shafi'i school's own self-justification.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored medium-high (0.58 at interval end) because the hierarchy's operation systematically transfers interpretive authority to a narrow specialist class (isnad critics) at the expense of other legitimate epistemic sources (communal practice, unconstrained reasoning, custom) that pre-existing schools treated as valid. Suppression (0.52) reflects that once codified as orthodox method, competing source-weighting became harder to sustain within mainstream legal discourse, though it never fully disappeared (rival schools persisted). Theater ratio is kept modest (0.28) — the coordination function (resolving genuine methodological chaos across early schools) is real and substantial, not primarily performative, even as the extraction layered onto it is also real.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith-transmission scholars and Shafi'i-trained jurists are the structural beneficiaries: the hierarchy makes their specific expertise indispensable and expands their institutional reach (d near beneficiary end). Medinan amal practitioners, independent qiyas jurists, and regional customary authorities are targets: their forms of authority are demoted to subordinate status by the same structure that elevates hadith criticism, and their exit options are constrained by the hierarchy's absorption into mainstream Sunni legal education (d near target end). The broader Sunni populace sits closer to symmetric: real coordination benefit (predictable rulings) offset by loss of locally responsive adjudication.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine methodological inconsistency across early legal schools — was real and is only partially resolved: contemporary legal pluralism among the four Sunni schools persists, meaning the hierarchy did not eliminate diversity so much as institutionalize one particular ranking of sources as authoritative within its own school. Classifying this as tangled_rope rather than pure snare prevents mislabeling: there is a genuine coordination function (shared method for weighing conflicting sources) bundled with genuine asymmetric extraction (concentration of authority in hadith-transmission specialists at the expense of other source-traditions) — collapsing it into either pure coordination or pure extraction would erase one of the two real components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standardization_vs_capture,
    'Did al-Shafi''i''s methodological hierarchy genuinely resolve a destabilizing inconsistency in early Islamic jurisprudence, or did it primarily transfer interpretive authority to a hadith-transmission specialist guild by recasting their technical skill as the necessary arbiter of legal validity?',
    'Comparative historical analysis of pre-Shafi''i legal ruling consistency versus post-standardization ruling consistency across regions, plus analysis of who held qadi and teaching appointments before and after the Risala''s adoption as normative method.',
    'If the historical record shows rulings were genuinely chaotic and became substantially more consistent without corresponding concentration of appointments among hadith specialists, the coordination function dominates and ε should be revised downward. If appointments and patronage concentrated sharply among isnad critics with limited consistency gain, extraction dominates and this reading moves toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_vs_capture, empirical, 'Whether standardization primarily solved inconsistency or primarily concentrated authority in hadith specialists.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel best framed as ''the four canonical sources of Islamic law'' (an ontological claim about what law derives from) or as ''the correct procedure for weighing sources when they conflict'' (a methodological/procedural claim)? The Shafi''i reading treats the ordering itself as near-canonical, but this may conflate a substantive claim about sources with a narrower procedural claim about conflict resolution.',
    'Textual analysis of al-Shafi''i''s Risala against later usul al-fiqh commentary to determine whether the strict ordering is presented as an ontological necessity or a pragmatic resolution device; compare with how rival schools frame their own methodological commitments.',
    'Under the ontological framing, sibling readings that admit additional independent sources (Maliki amal, Hanbali Companion-opinion) would be in tighter tension with this reading (closer to forecloses); under the narrower procedural framing, the readings are more clearly coexisting alternative procedures for the same underlying evidentiary base (coexists_with), which is the framing adopted here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is an ontological claim about valid sources or a narrower procedural claim about conflict resolution among them.').

omega_variable(
    hadith_authentication_reliability,
    'How reliable is the isnad-criticism apparatus itself as an arbiter — does rigorous chain-of-transmission scholarship reliably distinguish authentic from fabricated hadith, or does it retain irreducible interpretive discretion that functions similarly to the qiyas/istihsan reasoning it is meant to constrain?',
    'Cross-comparison of hadith authentication disputes across centuries and schools; assessment of how often isnad criticism produces consensus versus contested rulings among specialists.',
    'If isnad criticism retains substantial discretion, the claim that it provides a more objective arbiter than qiyas is weakened, and the extraction captured by hadith-transmission scholars looks less justified by superior methodological rigor and more like guild capture under a different vocabulary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hadith_authentication_reliability, empirical, 'Whether hadith authentication is meaningfully more objective than the reasoning methods it is meant to discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__shafii_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__shafii_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__shafii_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__shafii_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__shafii_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'the correct method of Islamic jurisprudence' (jurisprudential_method_kernel). The Hanafi, Maliki, Hanbali, and Shafi'i readings each authorize a different source-hierarchy and therefore a different beneficiary/victim structure and a different ε; per the ε-invariance principle they are authored as four separate files, linked by network edges, rather than as one story with an internal 'which school' parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
