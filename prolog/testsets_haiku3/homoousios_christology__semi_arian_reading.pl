% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Homoiousios Christology: Semi-Arian Reading
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The homoiousios ('of similar substance') Christology is a compromise
 *   position developed by moderate bishops in the Eastern empire during the
 *   mid-4th century (approximately 357–381 CE), in response to the fracturing
 *   aftermath of the Council of Nicaea (325). The Nicene formula homoousios
 *   ('one substance' or 'consubstantial') had been intended to settle the
 *   Christological question but instead divided the episcopate. Arian bishops
 *   rejected it as non-scriptural and philosophically presumptuous. Many
 *   moderate bishops found themselves sympathetic to both the Nicene
 *   commitment to Christ's divinity and the Arians' concerns about the
 *   metaphysical claims involved. Homoiousios offered a via media: Christ is
 *   of *similar* divine substance with the Father, affirming his divinity and
 *   uniqueness without claiming absolute identity. This reading instantiates
 *   the semi-arian settlement as perceived FROM INSIDE the moderate Episcopal
 *   coalition that defended it.
 *
 * KEY AGENTS:
 *   - Moderate episcopate (Basil of Ancyra, Eusebius of Caesarea, council-negotiating bishops): primary architects and defenders of homoiousios
 *   - Imperial court and ecclesiastical administrators: enforce the formula as a stability mechanism
 *   - Pro-Nicene episcopate (Athanasius, later Cappadocians): experience homoiousios as inadequate retreat
 *   - Arian bishops: formally excluded despite the language of similarity
 *   - Ecumenical councils: the institutional loci where the formula is authored and contested
 *   - Western Latin church: maintain Nicene homoousios, tolerate homoiousios for communion's sake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.38).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.42).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Homoiousios Christology: Semi-Arian Reading").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, 'e8c36b47-5d33-4c59-946c-01817d98147a').
narrative_ontology:cs_kernel_codification('e8c36b47-5d33-4c59-946c-01817d98147a', formalized).
narrative_ontology:cs_authority_grounding('e8c36b47-5d33-4c59-946c-01817d98147a', lineage).
narrative_ontology:cs_interpretation_layer_present('e8c36b47-5d33-4c59-946c-01817d98147a').
narrative_ontology:cs_reading_relation('e8c36b47-5d33-4c59-946c-01817d98147a', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('e8c36b47-5d33-4c59-946c-01817d98147a', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_axiom('e8c36b47-5d33-4c59-946c-01817d98147a', foundational, christ_similar_not_identical_substance).
narrative_ontology:cs_axiom_status(christ_similar_not_identical_substance, overridden).
narrative_ontology:cs_axiom_grounding('e8c36b47-5d33-4c59-946c-01817d98147a', christ_similar_not_identical_substance, deontological).
narrative_ontology:cs_axiom('e8c36b47-5d33-4c59-946c-01817d98147a', secondary, imperial_communion_over_doctrinal_precision).
narrative_ontology:cs_axiom_status(imperial_communion_over_doctrinal_precision, overridden).
narrative_ontology:cs_axiom_grounding('e8c36b47-5d33-4c59-946c-01817d98147a', imperial_communion_over_doctrinal_precision, conventional).
narrative_ontology:cs_reference_frame('e8c36b47-5d33-4c59-946c-01817d98147a', moderate_episcopal_consensus).
narrative_ontology:cs_drift_state('e8c36b47-5d33-4c59-946c-01817d98147a', constantinople_381, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('e8c36b47-5d33-4c59-946c-01817d98147a', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, moderate_episcopate).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_stability_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, western_latin_church).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, pro_nicene_episcopate).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, western_latin_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and theologians who hold that Christ is of similar but not identical substance with the Father. They set the terms of ecclesiastical debate through councils and episcopal consensus, seeking a formula that avoids both Arianism's subordinationism and the Pro-Nicene identity claim they see as logically problematic or excessively rigid. Their authority rests on patristic interpretation and the pragmatic need to maintain communion across the empire.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, moderate_episcopate, agenda_setter,
    organized, generational, constrained, continental).

% The imperial court and its ecclesiastical functionaries, who benefit from a compromise Christology that can hold the Eastern and Western episcopates in a single communion without schism. Homoiousios operates as a workable lingua franca that neither fully commits the empire to Nicene rigidity nor admits Arian subordinationism. Imperial enforcement of the compromise is instrumental to political unity.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, imperial_stability_apparatus, beneficiary,
    institutional, generational, mobile, continental).

% Bishops committed to full consubstantiality (homoousios). They experience homoiousios as a retreat from the Nicene confession and a dangerous softening that leaves room for Arian reinterpretation. They must negotiate and often temporarily concede to the compromise to avoid total schism, even as they maintain that the formula falls short of christological truth.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, pro_nicene_episcopate, payer,
    powerful, generational, constrained, continental).

% Bishops who hold that Christ is created and subordinate. From their perspective, homoiousios is a deceptive formula that uses similarity language to obscure the persistence of Nicene identity-claims; the formula systematically excludes them from communion while appearing to offer a middle path. Their exclusion from the homoiousios settlement is what the arrangement's enforcement maintains.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, arian_bishops, excluded,
    powerful, generational, trapped, continental).

% The Latin-speaking Western church (Rome, North Africa, Gaul) which never fully accepted homoiousios — they held to Nicene homoousios throughout. They experience the Eastern compromise as a concession they are forced to tolerate for the sake of imperial communion, yet they also benefit from the fact that homoiousios is closer to Nicene orthodoxy than open Arianism and therefore maintains some doctrinal continuity.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, western_latin_church, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, western_latin_church, beneficiary).

% Monks, ascetics, and popular theologians attached to the homoousios confession as a non-negotiable inheritance from Nicaea. They experience homoiousios as a betrayal of the Council's decision and view its proponents as crypto-Arians wearing a moderate mask. Their exclusion from the episcopate's bargaining table is maintained by the institutional structure of episcopal authority.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, theological_traditionalists, excluded,
    moderate, generational, identity_locked, continental).

% Imperial-convened assemblies of bishops tasked with establishing the empire-wide doctrinal settlement. They are the formal venue for negotiating and ratifying christological definitions. Their pronouncements carry imperial enforcement authority; they are where the constraint is authored and declared binding.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, ecumenical_councils, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, ecumenical_councils, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.15 at 325, immediately post-Nicaea) because the problem has just crystallized: the council's decision has not yet divided the episcopate along settled lines, and no coordinated alternative formula yet exists. By 357 (Council of Sirmium, where homoiousios gains explicit formulation), extractiveness rises to 0.38 as the moderate coalition begins to enforce the formula against both Arian and Pro-Nicene pressure. It plateaus around 0.37–0.41 from 365 onward as the constraint stabilizes. Theater ratio rises sharply from 0.08 (325) to 0.27 (365), indicating that by the later 360s, the moderate formula requires increasing amounts of interpretive labor and rhetorical defense to hold together: bishops must insist the formula is not Arian while also insisting it is not rigid Nicenism. The measurement at 381 (the Council of Constantinople) shows a slight drop in both metrics: the Pro-Nicene reading has been reasserted as orthodox, and homoiousios is being absorbed back into the Pro-Nicene framework (a reclassification that retroactively redefines the constraint). The suppression metrics track the imperial and ecclesiastical enforcement needed to maintain homoiousios against exclusionary pressure from both directions: Arians are expelled, Pro-Nicenes are pressured to accept it, and traditionalists are marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the moderate Episcopal seat, homoiousios is a genuine coordination mechanism addressing a real theological and political problem. From the Pro-Nicene seat, it is a defection and doctrinal softening. From the Arian seat (excluded), it is a deceptive formula that uses 'similarity' language to obscure Nicene identity claims while appearing to offer compromise. From the imperial seat, it is a pragmatic success — non-schism — that requires active maintenance but delivers political benefit. The engine should compute these seats as divergent: the moderate episcopal seat should compute the constraint as Rope (coordinating divided bishops without schism); the Pro-Nicene seat should compute it as Snare or Tangled Rope (enforced extraction of doctrinal concession); the Arian seat as Snare (forced exclusion under false compromise language); the imperial seat as Rope (coordination benefit justifies enforcement cost). These divergences are structural, not errors.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the moderate episcopate sits near 0.4–0.5 (slightly beneficiary, given their role in authoring and maintaining the formula, but constrained by the need to defend it continuously against both directions). Pro-Nicenes sit near 0.65–0.75 (target: they pay the cost of accepting 'similarity' rather than 'identity'). Arians sit at 1.0 (full targets: they are excluded entirely). The imperial apparatus sits near 0.2 (beneficiary: collects non-schism, invests in enforcement). The West sits near 0.6 (target: pressured to abandon Nicene clarity). No directionality override is needed; the structural data (beneficiary declarations for moderates and empire, victims/payers for Pro-Nicenes and the West, excluded status for Arians) produce accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   Homoiousios undergoes rapid mandatrophy. The founding problem (how to maintain communion across irreconcilable Christological positions) is live for roughly two decades (357–381). By 381, the Second Council explicitly reasserts Nicene homoousios as the authoritative standard and retroactively reinterprets homoiousios as compatible with it — or obsolete. The moderate bishops' original intent (similarity as distinct from identity) is officially foreclosed. The constraint does not disappear, but its mandate does: it transforms from a live compromise into an absorbed-and-reinterpreted artifact. This is the canonical case of mandatrophy by canonical override. The theater ratio at 381 drops slightly (to 0.28) because the constraint is no longer actively defended as a separate position; instead, it is made to mean what the victorious Pro-Nicene reading says it means. The contradiction between the founding problem's live status (the bishops genuinely sought communion) and the disappearance verdict (if homoiousios vanished, schism would result) gets resolved by recognizing that the constraint's persistence after 381 is inertial, not functional: it survives as a historical artifact and as a bridge language for absorbing former moderate bishops into Pro-Nicene communion, not as an active coordination mechanism. The theater ratio's rise to 0.27–0.31 and subsequent drop reflects this: the formula requires maximum interpretive labor at 365–373 to hold together; by 381 it is no longer held together, it is reinterpreted into coherence with its victors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    similarity_identity_boundary_ambiguity,
    'Is ''similar substance'' (homoiousios) logically distinct from ''identical substance'' (homoousios), or is the distinction merely rhetorical/philosophical and not substantive?',
    'Logical analysis of the Cappadocian reinterpretation of homoiousios (post-381): the Cappadocians (Basil, Gregory of Nyssa) argue that similarity entails identity in the theological context. If their reinterpretation is accepted, the boundary collapses; if rejected as eisegesis, the boundary holds.',
    'If the boundary is merely rhetorical, homoiousios is a linguistic compromise without real doctrinal content, making the constraint''s classification tilt toward Piton (performative maintenance). If the boundary is substantive, the constraint is a genuine coordination mechanism between different theological frameworks, keeping it as Rope or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(similarity_identity_boundary_ambiguity, conceptual, 'Whether the semi-Arian distinction between similarity and identity is conceptually stable or collapses under scrutiny.').

omega_variable(
    imperial_coercion_vs_episcopal_consensus,
    'How much of homoiousios'' persistence is due to imperial enforcement, and how much is due to genuine episcopal consensus that the formula serves their interests?',
    'Counterfactual analysis: would the moderate bishops continue to defend homoiousios without imperial backing (as the West initially did with Nicene homoousios)? Historical evidence from periods of relaxed imperial pressure (particularly the reign of Julian the Apostate, 361–363) shows bishops still defending homoiousios, suggesting consensus exceeds coercion alone.',
    'If consensus substantially exceeds coercion, the suppression metric should be lower and the constraint should classify more clearly as Rope. If coercion is primary, suppression is correctly high and the constraint tilts toward Tangled Rope or Snare. The evidence suggests consensus is real but not sufficient without imperial enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_coercion_vs_episcopal_consensus, empirical, 'The balance between imperial enforcement and voluntary episcopal consensus in maintaining homoiousios.').

omega_variable(
    theological_cover_vs_political_function,
    'Is homoiousios primarily a theological position (a genuine attempt to resolve Christological questions) or primarily a political compromise (a language game that permits communion without resolution)?',
    'Read the moderate bishops'' theological writings (Basil of Ancyra, Eusebius of Caesarea) and assess whether their defense of similarity engages with scriptural and patristic texts sincerely, or whether the theological apparatus is post-hoc rationalization for a political settlement. Parallel reading of Pro-Nicene and Arian theological literature to assess whether the moderates'' theological claims are treated as serious positions or as transparent compromises.',
    'If primarily theological, the constraint should be classified as a genuine Rope coordinating bishops across real disagreement. If primarily political, the theater ratio should be higher and the constraint should tilt toward Piton (atrophied function maintained for its political utility). The measurement series shows theater ratio rising to 0.27, suggesting increasing performative labor — consistent with political-function primacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_cover_vs_political_function, conceptual, 'Whether homoiousios is a sincere theological position or a rationalized political compromise.').

omega_variable(
    reading_absorption_irreversibility,
    'Once the Pro-Nicene reading absorbs homoiousios (post-381), can the semi-Arian reading be reconstructed, or is it permanently foreclosed?',
    'Examine whether any post-381 defenders of homoiousios attempt to assert its distinct meaning against the Cappadocian reinterpretation, or whether the reading disappears from active defense. Historical evidence shows the reading is abandoned within a generation — no 5th-century bishop defends ''similarity'' as distinct from ''identity''. This suggests foreclosure is asymmetric: the Pro-Nicene reading foreclosed the semi-Arian reading, not vice versa.',
    'This is the defining fact of mandatrophy: the founding problem (maintaining communion across Christological difference) was solved by the Pro-Nicene victory, not by the semi-Arian compromise. The constraint persists as an absorbed formula, not as an active coordination mechanism. Mandatrophy is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_absorption_irreversibility, empirical, 'The historical irreversibility of the Pro-Nicene absorption of homoiousios.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__semi_arian_reading, theater_ratio, 325, 0.08).
narrative_ontology:measurement(homo_tr_t340, homoousios_christology__semi_arian_reading, theater_ratio, 340, 0.12).
narrative_ontology:measurement(homo_tr_t357, homoousios_christology__semi_arian_reading, theater_ratio, 357, 0.22).
narrative_ontology:measurement(homo_tr_t365, homoousios_christology__semi_arian_reading, theater_ratio, 365, 0.27).
narrative_ontology:measurement(homo_tr_t373, homoousios_christology__semi_arian_reading, theater_ratio, 373, 0.31).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__semi_arian_reading, theater_ratio, 381, 0.28).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__semi_arian_reading, base_extractiveness, 325, 0.15).
narrative_ontology:measurement(homo_be_t340, homoousios_christology__semi_arian_reading, base_extractiveness, 340, 0.24).
narrative_ontology:measurement(homo_be_t357, homoousios_christology__semi_arian_reading, base_extractiveness, 357, 0.38).
narrative_ontology:measurement(homo_be_t365, homoousios_christology__semi_arian_reading, base_extractiveness, 365, 0.41).
narrative_ontology:measurement(homo_be_t373, homoousios_christology__semi_arian_reading, base_extractiveness, 373, 0.37).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__semi_arian_reading, base_extractiveness, 381, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__semi_arian_reading, suppression_requirement, 325, 0.18).
narrative_ontology:measurement(homo_su_t340, homoousios_christology__semi_arian_reading, suppression_requirement, 340, 0.28).
narrative_ontology:measurement(homo_su_t357, homoousios_christology__semi_arian_reading, suppression_requirement, 357, 0.42).
narrative_ontology:measurement(homo_su_t365, homoousios_christology__semi_arian_reading, suppression_requirement, 365, 0.44).
narrative_ontology:measurement(homo_su_t373, homoousios_christology__semi_arian_reading, suppression_requirement, 373, 0.41).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__semi_arian_reading, suppression_requirement, 381, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__semi_arian_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three constraint stories, one per major reading: (1) semi_arian_reading (this file) — the moderate compromise, homoiousios; (2) pro_nicene_reading — the Nicene identity claim, homoousios; (3) arian_reading — the subordinationist reading, Christ as created. All three are readings of the same kernel (the nature of Christ's substance). They are not measurable along a single ε because they instantiate different constraints (different beneficiaries, different suppression mechanisms, different founding problems). The three stories share the same historical interval (325–381) and the same ecclesiastical domain, but differ in their ε values (semi-Arian ε ≈ 0.38; Pro-Nicene ε ≈ 0.25–0.30 before 381, near 0.0 post-381 within Pro-Nicene framing; Arian ε ≈ 0.55–0.65). The semi-Arian reading claims to influence both sibling readings by providing a middle position that neither fully satisfies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
