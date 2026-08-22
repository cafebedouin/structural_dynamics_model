% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Statehood Criteria — Declaratory Reading
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   The declaratory reading of the Montevideo Convention (Article 1) holds
 *   that an entity meeting four objective criteria — permanent population,
 *   defined territory, government, and capacity to enter relations with other
 *   states — is a state as a matter of law, independent of recognition by
 *   other states. This reading operates as a coordination mechanism for the
 *   international legal order: it supplies a self-executing test for
 *   statehood that functions even when powerful states withhold recognition
 *   for political reasons. The constraint is claimed as a rope because it
 *   coordinates legal identification without extracting from those it
 *   governs; the primary structural beneficiaries are de facto authorities
 *   and their populations, while parent states lose discretionary gatekeeping
 *   power. The reading is contested by the constitutive reading (statehood
 *   requires recognition) and the hybrid reading (statehood requires criteria
 *   plus normative legitimacy).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.18).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.35).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Statehood Criteria — Declaratory Reading").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy/state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '36540a9f-72ee-4440-9648-83c68520a7aa').
narrative_ontology:cs_kernel_codification('36540a9f-72ee-4440-9648-83c68520a7aa', formalized).
narrative_ontology:cs_authority_grounding('36540a9f-72ee-4440-9648-83c68520a7aa', lineage).
narrative_ontology:cs_interpretation_layer_present('36540a9f-72ee-4440-9648-83c68520a7aa').
narrative_ontology:cs_reading_relation('36540a9f-72ee-4440-9648-83c68520a7aa', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('36540a9f-72ee-4440-9648-83c68520a7aa', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('36540a9f-72ee-4440-9648-83c68520a7aa', foundational, statehood_attaches_automatically_to_criteria).
narrative_ontology:cs_axiom_status(statehood_attaches_automatically_to_criteria, holdable).
narrative_ontology:cs_axiom_grounding('36540a9f-72ee-4440-9648-83c68520a7aa', statehood_attaches_automatically_to_criteria, conventional).
narrative_ontology:cs_axiom('36540a9f-72ee-4440-9648-83c68520a7aa', foundational, recognition_is_declaratory_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_declaratory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('36540a9f-72ee-4440-9648-83c68520a7aa', recognition_is_declaratory_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('36540a9f-72ee-4440-9648-83c68520a7aa', montevideo_convention_article_1_text).
narrative_ontology:cs_drift_state('36540a9f-72ee-4440-9648-83c68520a7aa', contemporary_state_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36540a9f-72ee-4440-9648-83c68520a7aa', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, aspirant_state_populations).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, international_legal_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, declaratory_theory_of_statehood).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, montevideo_convention_article_1).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, self_determination_principle).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, legal_objectivity_of_statehood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control territory and population meeting Montevideo criteria but denied recognition by parent states. The declaratory reading validates their statehood as a legal fact, granting them treaty-making capacity, diplomatic immunities, and standing in international forums without needing consent from the recognizing states that withhold it. Their exit from the constraint is impossible — they are the claimants the reading exists to serve.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities, beneficiary,
    organized, biographical, constrained, regional).

% The populations governed by de facto authorities who meet objective criteria but lack recognition. Under the declaratory reading, their political community attains legal personhood automatically, unlocking human rights protections, development aid eligibility, and the right to self-determination as legal subjects rather than objects of parent state policy. They cannot exit the constraint; they live inside its consequences.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, aspirant_state_populations, beneficiary,
    powerless, generational, trapped, local).

% Existing recognized states that withhold recognition from qualifying entities to maintain territorial integrity, strategic leverage, or domestic political coalitions. The declaratory reading strips their conditional recognition power — they lose the ability to gatekeep statehood as a political tool. They bear the cost of the reading's operation through diminished sovereign discretion, but retain arbitrage-grade exit: they can ignore the reading in practice (non-recognition persists de facto) and absorb the reputational cost.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, parent_states, agenda_setter).

% The system of international law itself, which gains coherence and predictability when statehood attaches to objective criteria rather than political consensus. The reading reduces doctrinal fragmentation, limits the role of power in legal status determination, and makes the legal order more self-executing. It collects no rents and bears no costs; it is the structural beneficiary of legal determinacy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_legal_order, beneficiary,
    analytical, civilizational, analytical, universal).

% States that actively condition recognition on political concessions (democratic reforms, border settlements, minority protections). The declaratory reading undermines their leverage by making statehood legally automatic. They can exit the constraint's discipline by simply continuing non-recognition — the reading has no enforcement mechanism — but pay a legitimacy cost in legal forums and scholarly discourse.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, recognition_withholding_states, agenda_setter,
    institutional, biographical, mobile, global).

% Judicial bodies that must decide whether an entity is a state for jurisdictional or merits purposes. The declaratory reading gives them an objective test to apply; the constitutive reading requires them to assess political recognition. They neither collect nor pay; they apply the rule the reading supplies.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_courts_and_tribunals, observer,
    institutional, biographical, analytical, global).

% Armed groups that control territory and population but lack the Montevideo criteria (no defined territory, no capacity to enter relations, or no effective government). They would claim statehood under a looser standard but are excluded by the four criteria. They have no voice in the reading's formulation and no exit from its exclusionary boundary.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, non_state_armed_groups, excluded,
    organized, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an objective, verifiable test for statehood that operates without requiring consensus from existing states — solving the coordination problem of how the international legal system identifies its own subjects when political recognition is withheld for strategic reasons.
% TRANSFER_FUNCTION: Moves the legal status of 'state' — with its attendant rights (treaty capacity, immunity, UN membership pathway) and obligations (international responsibility, human rights duties) — from the discretionary grant of recognizing states to the automatic consequence of meeting four observable criteria (permanent population, defined territory, government, capacity to enter relations). The transfer is from political gatekeepers to legal criteria.
% ABSENT_VOICES: Populations in entities that meet some but not all criteria (e.g., effective government but contested territory) — they would argue for a more flexible standard but are not represented in the Montevideo formulation. Also absent: states that would extend recognition based on normative legitimacy (human rights, democracy) rather than effective control — the hybrid reading's constituency.
% DISAPPEARANCE_RATIONALE: If the declaratory reading vanished overnight, statehood would revert to a purely political act of recognition by existing states. Entities currently claiming statehood on objective grounds (Taiwan, Palestine, Somaliland, Kosovo, etc.) would lose their legal standing as automatic statehood claimants; their status would become entirely contingent on the political calculus of recognizers. The international legal order would lose its only objective membership criterion.
% FOUNDING_PROBLEM: The collapse of empires and the proliferation of new political entities after WWI created a vacuum: no agreed method to determine which entities were states. The Montevideo Convention (1933) codified the declaratory theory to replace the chaotic practice of ad hoc recognition with an objective legal standard.
% FOUNDING_PROBLEM_CORROBORATION: The Montevideo Convention itself and the declaratory theory's scholarly defenders (Kelsen, Lauterpacht, Crawford) attest the founding problem was the need for legal objectivity in state identification. Constitutive theorists (traditional diplomatic practice, many UN members in the decolonization era) and hybrid theorists (contemporary human rights conditionality advocates) attest the problem was never purely legal — political legitimacy always mattered. No single corroborating source outside the declaratory tradition affirms the founding problem as purely objective; the contestation is the point.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).
:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18) because the reading does not transfer resources from payers to beneficiaries — it transfers legal status from political discretion to objective criteria. The 'cost' to parent states is loss of leverage, not resource extraction. Suppression (0.35) reflects the reading's lack of enforcement: non-recognizing states simply ignore it, and the reading cannot compel recognition. Theater ratio (0.22) captures the performative invocations of the criteria by entities seeking recognition while powerful states maintain non-recognition regardless. Accessibility collapse (0.42) is moderate: alternatives (constitutive, hybrid) persist as live readings. Resistance (0.58) is significant: the reading faces active doctrinal and political pushback from states that benefit from conditional recognition.
 *
 * PERSPECTIVAL GAP:
 *   From the de facto authority's seat, the reading is a rope — genuine coordination that grants legal standing without extraction. From the parent state's seat, the same reading operates as a constraint on sovereign discretion — a limitation on their traditional prerogative to recognize or not. The engine computes this seat divergence from the structural data; the authored claim (rope) reflects the reading's self-understanding, not the parent state's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities and aspirant populations are structural beneficiaries (d near 0.0): the reading grants them legal personhood automatically. Parent states and recognition-withholding states are payers (d ~0.6-0.7): they lose conditional recognition leverage but retain de facto non-recognition as exit. The international legal order and courts are analytical/beneficiary seats (d ~0.1): they gain coherence without bearing costs. Non-state armed groups are excluded (no d computation): they fall outside the criteria entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (chaotic recognition practice after WWI) has partially resolved — decolonization largely used the declaratory framework — but the reading persists because the problem recurs whenever new entities emerge in contested contexts (post-Soviet, post-Yugoslav, secessionist). The reading has not atrophied into a piton because it still performs its coordination function for new claimants; it has not become a snare because it extracts no resources. It remains a live rope with contested domain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_vs_constitutive_foreclosure,
    'Does the declaratory reading''s core premise (statehood attaches automatically to criteria) logically foreclose the constitutive reading (statehood requires recognition) within any single coherent legal framework, or do they coexist as applicable to different contexts (e.g., declaratory for UN membership, constitutive for bilateral relations)?',
    'Analyze state practice and ICJ jurisprudence: if states and courts treat the readings as context-dependent tools rather than mutually exclusive theories, they coexist; if any framework adopting one must reject the other''s core premise, they foreclose.',
    'If foreclosure holds, the kernel contains a genuine structural contradiction — adopting one reading displaces the other. If coexistence holds, the kernel''s contestation is a policy choice, not a logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_vs_constitutive_foreclosure, conceptual, 'Whether the declaratory and constitutive readings are logically incompatible or contextually complementary.').

omega_variable(
    criteria_objectivity_ambiguity,
    'Are the four Montevideo criteria (population, territory, government, capacity) genuinely objective and verifiable, or do they contain embedded normative judgments (e.g., ''government'' implying effectiveness and legitimacy, ''capacity'' implying willingness to comply with international law)?',
    'Examine borderline cases (Somaliland, Taiwan, Palestine, Kosovo) where criteria satisfaction is contested: if disputes center on factual observation, criteria are objective; if disputes center on legitimacy assessments, criteria embed normativity.',
    'If criteria embed normativity, the declaratory reading covertly imports the hybrid reading''s normative conditions, collapsing the structural distinction between them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criteria_objectivity_ambiguity, conceptual, 'Whether the declaratory reading''s objective criteria are truly value-free or smuggle in normative legitimacy requirements.').

omega_variable(
    enforcement_gap_vs_coordination_purity,
    'Does the declaratory reading''s lack of enforcement mechanism (non-recognizing states face no legal sanction) mean it fails as a coordination mechanism, or is its coordination function purely epistemic (providing a focal point for legal argument) rather than behavioral?',
    'Measure correlation between declaratory criteria satisfaction and eventual recognition outcomes over time: if criteria-satisfying entities eventually gain recognition regardless of initial withholding, the reading coordinates expectations; if recognition remains permanently political, the reading is epistemic theater.',
    'If purely epistemic, the reading''s claimed coordination function is illusory — it would reclassify toward piton (theatrical maintenance of a coordination claim without behavioral effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_vs_coordination_purity, empirical, 'Whether the declaratory reading''s coordination function is behavioral (shapes state practice) or purely epistemic (structures legal argument).').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading (declaratory_reading) of the contested kernel montevideo_statehood_criteria. Sibling readings: constitutive_reading, hybrid_reading. The declaratory reading''s structural delta: de facto authorities enter victim set under recognition denial; parent states lose structural leverage to condition recognition; international law becomes self-executing rather than consensus-dependent.',
    'The kernel''s contestation is resolved only when a single framework (treaty, court, customary law) adopts one reading as authoritative — which has not occurred. The kernel remains an open structural fracture in international law.',
    'Documents the committer-frame structure for meta-analysis: this file is one reading, not the kernel. Other readings are separate constraint files linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Commitment-system framing: this constraint instantiates one reading of a contested kernel; the kernel itself is the stabilized commitment (Montevideo Convention Article 1) that generates multiple constraint stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.08).
narrative_ontology:measurement(mont_tr_t1945, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(mont_tr_t1991, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1991, 0.28).
narrative_ontology:measurement(mont_tr_t2008, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.12).
narrative_ontology:measurement(mont_be_t1945, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(mont_be_t1991, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1991, 0.22).
narrative_ontology:measurement(mont_be_t2008, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2008, 0.18).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.15).
narrative_ontology:measurement(mont_su_t1945, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(mont_su_t1991, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1991, 0.45).
narrative_ontology:measurement(mont_su_t2008, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, information_standard).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__declaratory_reading, 0.02).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, un_membership_admission).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, diplomatic_recognition_practice).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, self_determination_implementation).

% DUAL FORMULATION NOTE:
% The montevideo_statehood_criteria kernel decomposes into three constraint stories: declaratory_reading (this file, claimed rope, low extraction, coordination via objective criteria), constitutive_reading (claimed tangled_rope — coordination via recognition consensus with extraction from non-recognized entities), hybrid_reading (claimed snare — normative conditionality as extraction cover). The declaratory reading is the upstream objective baseline; the other two add political layers that increase extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__declaratory_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
