% ============================================================================
% CONSTRAINT STORY: parliament_act_1911__delegated_continuation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parliament_act_1911__delegated_continuation_reading, []).

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
 *   constraint_id: parliament_act_1911__delegated_continuation_reading
 *   human_readable: Parliament Act 1911: Delegated Legislation Reading
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The 1911 Parliament Act settled (provisionally) the constitutional
 *   conflict between the elected Commons and the hereditary Lords by
 *   substituting a two-reading procedure (Commons + Lords delay, not veto)
 *   for the previous bicameral consensus requirement. But the legal
 *   settlement itself remains contested. The delegated_continuation_reading
 *   interprets Acts passed under the 1911 procedure as subordinate
 *   legislation whose status is lower than traditional enactments passed
 *   through both chambers. This reading subordinates the Commons' expedited
 *   will to the conceptual framework of delegated legislation — a category
 *   designed for administrative rulemaking, not sovereign statute. The
 *   reading preserves judicial supervisory jurisdiction (courts may examine
 *   Parliament Act statutes more freely than traditional Acts) and maintains
 *   the theoretical dignity of bicameral review (the shortcut is subordinate,
 *   requiring justification). The constraint exhibits tangled rope structure:
 *   the reading coordinates preservation of constitutional review doctrine
 *   with the 1911 compromise, while extracting from single-chamber
 *   majoritarianism by keeping Parliament Act statutes perpetually
 *   subordinate to common law examination. The tension surfaces in R
 *   (Jackson) where courts had to confront whether a Parliament Act statute
 *   could be examined for vires under the 1911 procedure itself.
 *
 * KEY AGENTS:
 *   - The Commons Majority: Primary victim (powerless/trapped under delegated reading) — their enacted will is classified as subordinate, subject to ongoing judicial review as though it were administrative rulemaking
 *   - The Common Law Judiciary: Primary beneficiary (institutional/arbitrage) — preserves supervisory jurisdiction and institutional prestige as body competent to examine even Parliament Act statutes
 *   - Bicameral Review Defenders: Secondary beneficiary (organized/constrained) — the delegated reading theoretically preserves the second chamber's constitutional relevance by framing any shortcut as subordinate
 *   - Constitutional Scholars: Organized observers (powerful/mobile) — articulate and contest the reading through academic argument and appellate advocacy
 *   - The Analytical Observer: Civilizational/universal perspective (analytical/analytical) — risks naturalizing a contestable doctrinal choice as a logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parliament_act_1911__delegated_continuation_reading, 0.48).
domain_priors:suppression_score(parliament_act_1911__delegated_continuation_reading, 0.62).
domain_priors:theater_ratio(parliament_act_1911__delegated_continuation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parliament_act_1911__delegated_continuation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(parliament_act_1911__delegated_continuation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(parliament_act_1911__delegated_continuation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parliament_act_1911__delegated_continuation_reading, tangled_rope).
narrative_ontology:human_readable(parliament_act_1911__delegated_continuation_reading, "Parliament Act 1911: Delegated Legislation Reading").
narrative_ontology:topic_domain(parliament_act_1911__delegated_continuation_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(parliament_act_1911__delegated_continuation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parliament_act_1911__delegated_continuation_reading, '36c5130b-d8ec-4123-a8af-43ec65cd1c6d').
narrative_ontology:cs_kernel_codification('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', formalized).
narrative_ontology:cs_authority_grounding('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', lineage).
narrative_ontology:cs_interpretation_layer_present('36c5130b-d8ec-4123-a8af-43ec65cd1c6d').
narrative_ontology:cs_reading_relation('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', parliament_act_1911__coerced_consent_reading, influences).
narrative_ontology:cs_reading_relation('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', parliament_act_1911__commons_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', foundational, parliamentary_enactments_admit_doctrinal_hierarchy).
narrative_ontology:cs_axiom_status(parliamentary_enactments_admit_doctrinal_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', parliamentary_enactments_admit_doctrinal_hierarchy, conventional).
narrative_ontology:cs_axiom('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', secondary, delegated_procedure_subordination_preserves_review_authority).
narrative_ontology:cs_axiom_status(delegated_procedure_subordination_preserves_review_authority, holdable).
narrative_ontology:cs_axiom_grounding('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', delegated_procedure_subordination_preserves_review_authority, instrumental).
narrative_ontology:cs_reference_frame('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', bicameral_constitutional_hierarchy).
narrative_ontology:cs_drift_state('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', jackson_and_post_referendum_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('36c5130b-d8ec-4123-a8af-43ec65cd1c6d', '').
narrative_ontology:cs_kernel_id(parliament_act_1911__delegated_continuation_reading, parliament_act_1911).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parliament_act_1911__delegated_continuation_reading, bicameral_review_defenders).
narrative_ontology:constraint_beneficiary(parliament_act_1911__delegated_continuation_reading, common_law_judiciary).
narrative_ontology:constraint_victim(parliament_act_1911__delegated_continuation_reading, commons_single_chamber_authority).
narrative_ontology:constraint_victim(parliament_act_1911__delegated_continuation_reading, democratic_majoritarian_will).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEMOCRATIC MAJORITARIAN WILL (SNARE) — The Commons majority cannot exit the delegated procedure classification without overturning the reading itself. Suppressed alternatives: treating Acts passed under the 1911 procedure as full primary legislation bearing equal dignity with Acts passed through both chambers. The delegation frame extractively subordinates elected will to judicial review norms designed for secondary legislation.
constraint_indexing:constraint_classification(parliament_act_1911__delegated_continuation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMON LAW JUDICIARY (ROPE) — Courts benefit from the delegated legislation reading because it preserves their supervisory jurisdiction over Parliament Act statutes. The reading coordinates judicial review norms with parliamentary procedure, maintaining the institutional prestige of the bench as a body capable of examining legislative products. Net beneficiary through preserved institutional authority.
constraint_indexing:constraint_classification(parliament_act_1911__delegated_continuation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: BICAMERAL REVIEW DEFENDERS (TANGLED ROPE) — Organized actors (Lords reform advocates, constitutional traditionalists) experience the delegated reading as both coordination and extraction. The reading coordinates preservation of second-chamber influence through judicial deference doctrine; it extracts from the Commons by re-opening legislative outcomes to court examination. Exit is constrained — overturning the reading requires sustained political will or explicit statutory amendment.
constraint_indexing:constraint_classification(parliament_act_1911__delegated_continuation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL SCHOLAR (TANGLED ROPE) — The academic community can articulate alternative readings (commons supremacy, coerced consent) and has some influence over judicial interpretation through appellate briefs and legal commentary. The delegated reading coordinates constitutional doctrine with common law traditions (secondary legislation review); it extracts from majoritarian supremacy theory. Powerful within epistemic circles; mobile between readings through publication and advocacy.
constraint_indexing:constraint_classification(parliament_act_1911__delegated_continuation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, the logical distinction between primary and delegated legislation is treated as a structural feature of legal systems: primary legislation is sovereign will; delegated legislation is subordinate rulemaking. This perspective naturalizes the conceptual boundary as immutable. However, the structural data contradicts the mountain classification — the reading's beneficiaries and enforcement requirements reveal this as a contingent doctrinal claim, not an inherent logical necessity.
constraint_indexing:constraint_classification(parliament_act_1911__delegated_continuation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parliament_act_1911__delegated_continuation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parliament_act_1911__delegated_continuation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parliament_act_1911__delegated_continuation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parliament_act_1911__delegated_continuation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(parliament_act_1911__delegated_continuation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The delegated reading extracts from Commons majoritarians by treating their expedited enactments as subordinate, perpetually subject to more intensive judicial review than statutes passed through both chambers. But the extraction is not total — the Commons' substantive will typically prevails; the cost is doctrinal status, not practical overrule. The trajectory shows rising extractiveness over the interval (0.32 → 0.48) because intensified judicial review (Jackson era onward) has made the subordinate status operationally consequential. Suppression (0.62): Moderate-high. The Commons majority cannot exit the delegated classification without either (a) overturning the doctrinal reading through constitutional reform or judicial reversal (high political cost), or (b) reverting to bicameral consensus (functionally impossible). The alternative framing — that Parliament Act statutes are full primary legislation — is suppressed in orthodox constitutional discourse, though not eliminated (commons_supremacy reading remains available). Theater ratio (0.58): Moderate. The delegated reading involves significant performative elements: courts speak as though they are examining subordinate legislation while deferring to substantive Parliamentary choices. But the reading is not purely theatrical — it has real downstream effects on judicial review scope and constitutional theory development. The rising trajectory (0.45 → 0.58) reflects how the performative gap has widened as courts have spoken more vigorously about Parliament Act scrutiny without materially invalidating substantive outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The Commons majority (powerless/trapped) experiences this reading as high extraction — their will is subordinated and their products are perpetually reviewable. The judiciary (institutional/arbitrage) experiences it as coordination — the reading preserves their supervisory role and maintains constitutional order. Bicameral defenders (organized/constrained) experience it as mixed — they benefit from the reading's preservation of judicial review as a proxy for second-chamber influence, but they are constrained because the reading does not restore actual veto power. Scholars (powerful/mobile) experience it as contestable — they can articulate the alternative readings through academic work and appellate briefs, but they are constrained by judicial deference and institutional inertia. The analytical observer risks seeing a natural law (the logical distinction between primary and delegated legislation) where the structural data reveals a beneficiary-backed doctrinal claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The delegated reading's directionality structure derives from explicit constitutional hierarchy: Acts passed under the full four-reading procedure are treated as primary legislation (full sovereign status), while Acts passed under the 1911 two-reading procedure are treated as subordinate (subject to examination as delegated legislation would be). This hierarchy is beneficiary-enforced by the judiciary (who gain supervisory jurisdiction) and defended by bicameral constitutionalists (who gain theoretical preservation of second-chamber influence). The Commons majority, lacking exit options (trapped in the constitutional framework), bears the extraction of perpetual subordinate status. The suppression is structural: there is no alternative procedure available to the Commons for bypassing the Lords without changing the constitutional text itself. The reading suppresses the commons_supremacy alternative by treating it as legally untenable, though it remains available as a normative claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The delegated reading avoids mandatrophy by coherently distinguishing between (a) the substantive outcome (Commons will typically prevails on policy) and (b) the doctrinal status (the procedure is subordinate, subject to review). The reading claims that a Parliament Act statute can be fully effective as law while remaining conceptually subordinate as a jurisprudential matter. This is coherent if (and only if) courts do not actually use the subordinate classification to override substantive outcomes — if they do, mandatrophy surfaces (the reading claims subordinate status while the courts enforce it as total dominance, collapsing the distinction). R (Jackson) approached this boundary: if courts had invalidated a Parliament Act statute on vires grounds, the reading would have failed mandatrophy. That courts ultimately deferred to the Commons on the substantive question (while speaking about Parliament Act scrutiny) preserved the coherence, but it revealed the reading's dependence on judicial restraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_equivalence_ambiguity,
    'Does the 1911 Act''s two-reading procedure produce output with the same constitutional status as four-reading enactment, or does the procedural shortcut create a subordinate product?',
    'Historical analysis of Parliamentary intent (Asquith''s statements, Lords reform debates); comparative analysis of how courts have treated Parliament Act statutes (deference, scrutiny, invalidation rates) vs traditional enactments; examination of whether any Parliament Act statute has been challenged as ultra vires on the grounds of the 1911 procedure itself',
    'If equivalent: the delegated reading is jurisprudentially indefensible — Acts passed under the 1911 procedure bear full primary status, and the reading naturalizes a contingent doctrinal fiction. If subordinate: the reading reflects a real constitutional distinction with valid supervision grounds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_equivalence_ambiguity, empirical, 'Whether 1911 Act outputs have equivalent constitutional status to traditional enactments').

omega_variable(
    judicial_review_scope_boundary,
    'What is the proper scope of judicial scrutiny over Parliament Act statutes? Is review limited to vires within the 1911 procedure, or do courts examine substantive legislative choices?',
    'Case law analysis (Jackson, recent constitutional review decisions); examination of whether courts have applied rationality review, proportionality, or substantive limits to Parliament Act statutes vs traditional Acts; comparison of judicial deference doctrine across the two categories',
    'If review scope is narrow: the delegated reading describes a technical procedural category with minimal extraction (Commons majority retains substantive authority). If review scope is broad: the reading enables substantial judicial override of majoritarian will (high extraction, validating snare classification from majorities'' perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_review_scope_boundary, empirical, 'Scope of judicial review over Parliament Act statutes').

omega_variable(
    reading_coherence_across_formulations,
    'Is this reading logically coherent, or does treating Parliament Act enactments as subordinate create tensions with other constitutional doctrines (parliamentary sovereignty, rule of law, judicial deference)?',
    'Constitutional doctrinal coherence analysis; identification of contradictions between the delegated reading and established principles; examination of how courts have rationalized the status without explicitly endorsing the ''delegated legislation'' label',
    'If incoherent: the reading is maintained by institutional inertia rather than principled argument (piton candidate). If coherent: the reading represents a legitimate constitutional theory with valid grounds (stronger tangled_rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coherence_across_formulations, conceptual, 'Internal logical coherence of delegated legislation reading').

omega_variable(
    reading_contest_structuring,
    'Which alternative reading (coerced_consent, commons_supremacy) most directly contradicts this reading''s core premise, and which merely occupies a different normative stance on the same procedural facts?',
    'Logical analysis of reading premises: does the coerced_consent reading (extraction through peer threat) rule out the delegated reading (subordinate procedure), or do they describe different aspects? Does the commons_supremacy reading (elected will prevails) foreclose the delegated reading, or can both coexist (delegated status + ultimate Commons authority)?',
    'If coerced_consent forecloses delegated reading: the three readings are not independent; one rules out another. If they coexist: all three remain live within a contested kernel. This affects the cs_structure.reading_relations declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_structuring, conceptual, 'Logical structure of contest between this reading and its siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parliament_act_1911__delegated_continuation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parl1911_tr_t0, parliament_act_1911__delegated_continuation_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(parl1911_tr_t30, parliament_act_1911__delegated_continuation_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(parl1911_tr_t60, parliament_act_1911__delegated_continuation_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(parl1911_be_t0, parliament_act_1911__delegated_continuation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(parl1911_be_t30, parliament_act_1911__delegated_continuation_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(parl1911_be_t60, parliament_act_1911__delegated_continuation_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(parl1911_su_t0, parliament_act_1911__delegated_continuation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(parl1911_su_t30, parliament_act_1911__delegated_continuation_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(parl1911_su_t60, parliament_act_1911__delegated_continuation_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parliament_act_1911__delegated_continuation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(parliament_act_1911__delegated_continuation_reading, parliament_act_1911__coerced_consent_reading).
narrative_ontology:affects_constraint(parliament_act_1911__delegated_continuation_reading, parliament_act_1911__commons_supremacy_reading).

% DUAL FORMULATION NOTE:
% The parliament_act_1911 kernel generates three structurally distinct constraint stories, one for each reading. Each reading instantiates a different epsilon value, beneficiary set, and extracted victim group. The delegated_continuation_reading (this file) treats Parliament Act enactments as subordinate; the commons_supremacy_reading treats them as full primary legislation; the coerced_consent_reading focuses on the extractive origins rather than the ongoing status. These are not three perspectives on one constraint — they are three constraints sharing a single formal kernel, with the reading contest structuring their relationship. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parliament_act_1911__delegated_continuation_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
