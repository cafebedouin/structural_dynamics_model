% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation: Meaning Fixed at Ratification
 *   domain: constitutional_law/interpretive_theory/legal_philosophy
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution asserts that
 *   constitutional meaning is fixed at the moment of ratification and that
 *   judicial interpretation must recover the original public understanding of
 *   the text. This constraint embodies a specific reading of the
 *   constitutional kernel — the claim that the Constitution has a determinate
 *   meaning that binds subsequent generations. The originalist reading
 *   interprets this kernel as requiring fidelity to historical evidence of
 *   what the ratifying public understood the text to mean. This story models
 *   the originalist reading as a pure constraint: it exhibits high
 *   suppression of alternative interpretive methods, produces significant
 *   extraction for rights claimants who cannot ground their claims in
 *   18th/19th century practice, and benefits a conservative legal movement
 *   that gains institutional dominance through the constraint. The
 *   measurement trajectory shows intensifying suppression and extractiveness:
 *   originalism has become more institutionally dominant and more hostile to
 *   adaptive interpretation over the 30-year interval (roughly 1994–2024),
 *   correlating with the movement from academic theory to dominant practice
 *   in federal judiciary appointments and constitutional law pedagogy.
 *
 * KEY AGENTS:
 *   - Conservative Legal Movement: Institutional beneficiary (institutional/arbitrage) — originalism provides principled justification for conservative jurisprudence and enables institutional dominance without requiring moral argument
 *   - Unenumerated Rights Claimants: Primary victim (powerless/trapped) — cannot articulate contemporary rights claims within originalist frame; systematic closure of interpretive pathways
 *   - Progressive Legal Movement: Secondary victim and identity-locked agent (organized/identity_locked) — bears institutional defeat; identity fused with opposition to originalism makes exit unthinkable despite structural possibility
 *   - Federal Judiciary: Institutional observer (institutional/arbitrage) — practices originalism performatively as post-hoc legitimation; piton classification reflects degraded functionality
 *   - Adaptive Rights Interpretation: Abstract victim (powerless/trapped) — entire interpretive methodology suppressed; cannot be exercised legitimately within originalist regime
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing originalism as consequence of popular sovereignty rather than contingent interpretive choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, snare).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Constitutional Interpretation: Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional_law/interpretive_theory/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, 'e81d8923-7c1d-49f5-a091-7ca3317e8a4b').
narrative_ontology:cs_kernel_codification('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', fixed_text).
narrative_ontology:cs_authority_grounding('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', lineage).
narrative_ontology:cs_interpretation_layer_present('e81d8923-7c1d-49f5-a091-7ca3317e8a4b').
narrative_ontology:cs_reading_relation('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', foundational, historical_evidence_authoritative).
narrative_ontology:cs_axiom_status(historical_evidence_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', historical_evidence_authoritative, empirically_contingent).
narrative_ontology:cs_reference_frame('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', constitutional_fidelity_through_original_understanding).
narrative_ontology:cs_drift_state('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', contemporary_supreme_court_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('e81d8923-7c1d-49f5-a091-7ca3317e8a4b', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, institutional_originalism).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, adaptive_rights_interpretation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNENUMERATED RIGHTS CLAIMANTS (SNARE) — Claimants asserting rights not explicitly enumerated or grounded in 18th/19th century practice (privacy, dignity, autonomy in contemporary contexts) face maximal suppression. The constraint traps them by declaring their interpretive pathways illegitimate in advance. No alternative framing of constitutional text is permitted from this perspective; only historical evidence counts. Cannot exit without abandoning the rights claim itself.
constraint_indexing:constraint_classification(us_constitution_text__originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE JUDGES AND SCHOLARS (TANGLED ROPE) — Experience mixed extraction and coordination. The originalist constraint coordinates historical research and textual rigor (genuine benefit), but also suppresses their preferred interpretive methods and constrains their judicial latitude. They bear extraction costs (career risk of being labeled non-originalist activists) while also contributing to the coordination function (improving historical understanding of constitutional text). Constrained exit: can choose alternative doctrines but face institutional and professional penalty.
constraint_indexing:constraint_classification(us_constitution_text__originalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSERVATIVE LEGAL MOVEMENT (ROPE) — Net beneficiary experiencing the constraint as pure coordination. Originalism provides a principle that justifies conservative jurisprudence without having to argue moral superiority. The constraint enables institutional dominance by naturalized method. Benefits from the constraint's suppression of adaptive interpretation. Institutional exit available but not exercised — arbitrage capacity dormant because the constraint serves institutional interests.
constraint_indexing:constraint_classification(us_constitution_text__originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL JUDICIARY (PITON) — The judiciary perceives originalism as a degraded decision rule maintained through institutional inertia. Many judges apply originalism performatively — selecting historical evidence that confirms predetermined outcomes while claiming textual fidelity. The constraint provides theater (the legitimation of judicial decisions through historical masquerade) rather than substantive constraint on judicial discretion. Theater ratio reflects that originalism, as institutionally practiced, is largely a post-hoc framing device for decisions made on other grounds.
constraint_indexing:constraint_classification(us_constitution_text__originalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, original public meaning at ratification might appear as an immutable anchor point: constitutional validity is grounded in the People's sovereign act at a fixed moment. Meaning cannot be different from what the ratifying public understood; subsequent generations cannot rewrite the Constitution without amendment. This perspective naturalizes a particular constitutional theory as an inescapable logical consequence of popular sovereignty. However, the beneficiary/victim structure contradicts the mountain classification — identifiable institutional actors benefit from this constraint, and identifiable rights claimants bear its costs. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(us_constitution_text__originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: PROGRESSIVE LEGAL MOVEMENT (IDENTITY-LOCKED SNARE) — The progressive legal identity is partly constituted through opposition to originalism and defense of adaptive rights interpretation. Even where exit is structurally possible (alternative constitutional theories available, career paths in progressive practice), the movement's identity fusion with 'defending the living Constitution' and 'protecting unenumerated rights' makes exit unthinkable from within the movement's frame. The constraint experiences maximal extraction (institutional defeat, suppression of preferred interpretive methods, entrapment in reactive posture) but identity lock prevents the movement from abandoning constitutional interpretation entirely — abandoning the Constitution would dissolve the movement's identity.
constraint_indexing:constraint_classification(us_constitution_text__originalist_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_text__originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_text__originalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_text__originalist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The original research measured origin at 0.35 (pre-1994 originalism was academic and marginal); measurement at time 15 shows 0.45 (Scalia's tenure and Federalist Society influence); time 30 shows 0.58 (post-Gorsuch and Barrett appointments, originalism now controls the Supreme Court majority). The constraint extracts through three mechanisms: (1) suppression of adaptive interpretation, preventing entire classes of rights claims from being articulated; (2) institutional dominance benefiting conservative legal movement while penalizing progressive scholarship and practice; (3) entrenchment through federal judiciary appointments that lock conservative judges into originalist commitments. Suppression (0.72): High. The originalist frame produces systematic suppression of alternative methods through institutional mechanisms: law school curriculum that teaches originalism as the legitimate method, bar exams that test originalist doctrine, judicial precedent favoring originalist approaches, and professional culture that marginalizes adaptive interpretation as illegitimate activism. Rights claimants cannot exit the constraint without abandoning their rights claims; adaptive interpretation cannot be practiced in mainstream jurisprudence without reputational penalty. Theater ratio (0.38): Low. Originalism, in institutional practice, functions as a real suppressive mechanism, not pure theater. The historical work is genuine, even where conclusions are contested. Judges cannot claim originalism while completely ignoring historical evidence. The theater component reflects that originalist judges often reach predetermined outcomes and then supply historical rationale (selection bias in evidence), but the performative element is smaller than in pure piton cases.
 *
 * PERSPECTIVAL GAP:
 *   The originalist constraint produces maximal perspectival divergence. The conservative legal movement sees pure coordination (Rope) — originalism provides coherence and principled decision-making. Rights claimants see pure extraction (Snare) — the constraint systematically forecloses their interpretive pathways. Progressive judges see mixed extraction and coordination (Tangled Rope) — they contribute to historical rigor while being suppressed in their preferred methods. The federal judiciary sees degraded ritual (Piton) — originalism provides legitimation theater while judges exercise substantive discretion. The progressive legal movement sees identity-entrapment (identity-locked Snare) — structurally possible to abandon constitutional interpretation, but identity fusion prevents exit. The analytical observer risks naturalizing originalism as immutable (Mountain), but the beneficiary/victim structure reveals it as a contingent institutional arrangement. The perspectival gap demonstrates that the same constitutional constraint appears as legitimate method (rope), immutable law (mountain), degraded ritual (piton), and oppressive regime (snare) depending on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from their structural relationship to the constraint. Conservative legal movement: beneficiary + institutional + arbitrage → low d → low/negative effective extraction (χ). They experience the constraint as enabling rather than extractive. Rights claimants: victims + powerless + trapped → high d → high effective extraction (χ). No exit available; they bear maximal extraction. Progressive judges: both beneficiary (of historical rigor) and victims (of suppressed methods) + moderate + constrained → moderate d → moderate effective extraction (χ). Institutional judiciary: beneficiary (legitimation theater) + institutional + arbitrage → low d, but piton classification reflects theater_ratio gate rather than low extraction. Progressive movement (identity-locked): victims + organized + identity_locked → derived d reflects victim status + cognitive entrapment; the identity lock prevents exercise of structural exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist constraint resolves mandatrophy by demonstrating that interpretation is not pure coordination nor pure extraction, but rather a framework that coordinates historical rigor while enabling institutional extraction. From the beneficiary's perspective (conservative movement), originalism is genuinely coordinative — it solves the problem of how to adjudicate constitutional cases with principled reasoning. From the victim's perspective (rights claimants), originalism is purely extractive — it forecloses their interpretive pathways without offering reciprocal benefit. From the analytical perspective, the constraint exhibits both real coordination function (historical rigor, textual fidelity) and real extraction mechanism (institutional dominance, suppression of alternatives). The mandatrophy is resolved by accepting that the same constraint can be coordinative from one position and extractive from another, depending on the beneficiary/victim structure. The constraint is not 'really' coordination or extraction — it is both, indexed to different observers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_epistemology,
    'Is original public meaning at ratification an epistemically discoverable fact or a construction imposed retroactively by contemporary interpreters?',
    'Meta-historical analysis of how historical evidence is selected and interpreted in originalist scholarship; comparison of competing originalist reconstructions of the same constitutional provision; examination of whether disagreement among originalist historians undermines claims of discovery vs. construction',
    'If discovered fact: originalism is an epistemologically constrained method, and the mountain perspective gains force. If constructed: originalism is a contingent interpretive choice, and the snare classification is appropriate throughout. This distinction determines whether the constraint is inevitable or contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_public_meaning_epistemology, conceptual, 'Whether original public meaning is discovered or constructed').

omega_variable(
    positivist_reading_logical_foreclosure,
    'Does the originalist reading''s commitment to ''meaning fixed at ratification'' logically foreclose the positivist reading''s claim that constitutional validity derives from formal enactment procedures independent of meaning?',
    'Logical analysis: can a single interpretive framework hold both that (a) meaning is fixed at ratification and (b) meaning is irrelevant to constitutional validity? Examination of whether originalists have engaged positivist arguments and on what grounds.',
    'If foreclosed: originalism and positivism cannot coexist in a single constitutional theory; the relation should be ''forecloses'' rather than ''coexists_with''. If not foreclosed: they address different questions (what does the text mean vs. what makes the text valid) and can coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_reading_logical_foreclosure, conceptual, 'Whether originalism logically rules out constitutional positivism').

omega_variable(
    living_constitutionalism_logical_incompatibility,
    'Are originalism and living constitutionalism logically incompatible (meaning cannot be both fixed and evolving in the same interpretive framework), or do they represent different decisions about the same text that different judges could make?',
    'Logical analysis of the core claims: does ''meaning fixed at ratification'' entail ''meaning cannot evolve''? Are there coherent hybrid positions (e.g., ''principles are fixed but their applications evolve'')? Examination of whether any constitutional scholar holds both positions simultaneously.',
    'If logically incompatible: relation is ''forecloses''. If both can be held by different judges/theorists in good faith: relation is ''coexists_with''. This distinction affects how the kernel contest is modeled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_constitutionalism_logical_incompatibility, conceptual, 'Whether originalism and living constitutionalism are logically incompatible').

omega_variable(
    suppression_mechanism_enforcement_vs_internalization,
    'Is the high suppression (0.72) primarily enforced through institutional mechanisms (precedent doctrine, judicial gatekeeping, law school curriculum) or internalized through professional identity formation (judges and lawyers who believe originalism is the correct method)?',
    'Analysis of how originalism is transmitted and maintained: institutional requirements (what do law schools teach? what do bar exams test?) vs. professional culture (do originalists adopt the method because they believe in it or because it is institutionally dominant?). Examination of whether originalist judges would adopt the method absent institutional pressure.',
    'If primarily institutional enforcement: the constraint is externally maintained and vulnerable to institutional change. If primarily internalized: the constraint is self-sustaining and more resistant to change. This affects the mechanism of potential exit for judges and scholars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_enforcement_vs_internalization, empirical, 'Whether suppression is institutionally enforced or internalized').

omega_variable(
    rights_claimants_interpretive_closure,
    'Can rights claimants articulate unenumerated rights claims within an originalist framework (discovering that the original public understood rights to include contemporary assertions), or does the originalist frame systematically preclude such claims?',
    'Historical and doctrinal analysis: examination of successful rights claims within originalist jurisprudence; comparison of originalist receptivity to different classes of rights claims; analysis of whether originalist method has ever expanded the scope of protected rights.',
    'If originalism is systematically closed to contemporary rights claims: the snare classification is robust. If originalism occasionally permits rights expansion (through historical discovery): the constraint is less purely extractive and might warrant tangled_rope reclassification from some victims'' perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_claimants_interpretive_closure, empirical, 'Whether originalist framework permits unenumerated rights discovery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(const_orig_be_t0, us_constitution_text__originalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(const_orig_be_t15, us_constitution_text__originalist_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(const_orig_be_t30, us_constitution_text__originalist_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(const_orig_su_t0, us_constitution_text__originalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(const_orig_su_t15, us_constitution_text__originalist_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(const_orig_su_t30, us_constitution_text__originalist_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, judicial_interpretive_discretion_suppression).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, unenumerated_rights_epistemic_closure).

% DUAL FORMULATION NOTE:
% The originalist reading is part of the us_constitution_text kernel constraint family. Other readings (living constitutionalist, positivist) are separate constraint stories with different ε values, beneficiary/victim structures, and classifications. The originalist reading here models one specific interpretation of constitutional meaning; sibling readings model alternative interpretations of the same constitutional text. The network links show how originalism influences (and is influenced by) adjacent constraints about judicial discretion and rights epistemology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__originalist_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
