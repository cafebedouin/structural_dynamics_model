% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animals as Legal Property Without Independent Moral Standing
 *   domain: legal/ethical/political-economy
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel animal_status. The
 *   property reading holds that animals lack independent moral standing and
 *   remain legal property subject only to explicitly legislated welfare
 *   constraints. The constraint is minimal: it declares no parties as
 *   structural victims (animals are excluded from the moral calculus by
 *   design), treats use restrictions as human-imposed welfare obligations
 *   rather than animal rights, and claims near-zero base extractiveness (ε =
 *   0.05) because no party is structurally exploited under the axiom that
 *   animals have no independent interests to exploit. The claim and metrics
 *   are intentionally aligned here: the property reading's core axiom
 *   determines what counts as extraction, and under that axiom, extraction is
 *   minimal. The sibling readings (welfare_reading, abolitionist_reading) are
 *   DIFFERENT CONSTRAINTS with different victim sets, beneficiary structures,
 *   and ε values; they are NOT perspectives on this constraint.
 *
 * KEY AGENTS:
 *   - property_owners_animal_use (institutional power, agenda-setter, collects use rights)
 *   - courts_and_legislatures (institutional power, agenda-setter, codify the property reading)
 *   - welfare_advocates (organized power, excluded, operate within property framework via welfare carve-outs)
 *   - abolitionist_advocates (moderate power, excluded, reject property axiom entirely)
 *   - consumers_animal_products (organized power, beneficiary, gain access via unrestricted use)
 *   - animals (non-agent, powerless, explicitly denied standing by design)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.12).
domain_priors:theater_ratio(animal_status__property_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animals as Legal Property Without Independent Moral Standing").
narrative_ontology:topic_domain(animal_status__property_reading, "legal/ethical/political-economy").

domain_priors:requires_active_enforcement(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'f0b211af-f241-4e24-bea3-5826a357ab8e').
narrative_ontology:cs_kernel_codification('f0b211af-f241-4e24-bea3-5826a357ab8e', fixed_text).
narrative_ontology:cs_authority_grounding('f0b211af-f241-4e24-bea3-5826a357ab8e', lineage).
narrative_ontology:cs_interpretation_layer_present('f0b211af-f241-4e24-bea3-5826a357ab8e').
narrative_ontology:cs_reading_relation('f0b211af-f241-4e24-bea3-5826a357ab8e', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0b211af-f241-4e24-bea3-5826a357ab8e', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('f0b211af-f241-4e24-bea3-5826a357ab8e', foundational, animals_lack_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('f0b211af-f241-4e24-bea3-5826a357ab8e', animals_lack_independent_moral_standing, conventional).
narrative_ontology:cs_axiom('f0b211af-f241-4e24-bea3-5826a357ab8e', foundational, property_rights_absolute_except_welfare_statute).
narrative_ontology:cs_axiom_status(property_rights_absolute_except_welfare_statute, holdable).
narrative_ontology:cs_axiom_grounding('f0b211af-f241-4e24-bea3-5826a357ab8e', property_rights_absolute_except_welfare_statute, conventional).
narrative_ontology:cs_reference_frame('f0b211af-f241-4e24-bea3-5826a357ab8e', animals_as_chattels_property).
narrative_ontology:cs_drift_state('f0b211af-f241-4e24-bea3-5826a357ab8e', contemporary_welfare_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0b211af-f241-4e24-bea3-5826a357ab8e', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, property_owners_animal_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, consumers_animal_products).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufacturers, agricultural enterprises, research institutions, and private owners who retain unrestricted use rights over animals except where explicit welfare statutes apply. They set standards for acceptable animal use, lobby for welfare statute thresholds that protect profit margins, and enforce exclusionary property claims against non-owners. They benefit from the constraint by retaining use and disposition rights without bearing the epistemic or moral burden of animal interests as independent claims.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, property_owners_animal_use, agenda_setter,
    institutional, generational, mobile, national).

% Animal welfare organizations and constituencies who reject the property reading's core premise — that animals lack independent standing — but operate within its legal framework by advancing welfare statutes and enforcement rather than legal personhood. They are excluded from the rule-setting agenda; their advocacy is channeled into narrow welfare carve-outs rather than structural recognition.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_advocates, excluded,
    organized, biographical, constrained, national).

% Activists and philosophers who reject both the property reading AND the welfare reading — they advocate for animal legal personhood and the cessation of instrumental use. They are doubly excluded: their core premise (animals are rights-holders) is foreclosed by the property reading's axioms, and their proposed remedies (prohibition of use) are not available within the property-rights framework.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, abolitionist_advocates, excluded,
    moderate, biographical, constrained, national).

% Legal institutions that codify the property reading into statute and common law. They set the boundary between unrestricted use (the default under property law) and restricted use (where welfare statutes apply). They maintain the constraint's enforcement by denying standing to animals themselves in litigation and defining welfare as a human obligation rather than an animal right.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Purchasers of animal products (food, materials, research outputs) who benefit from the property reading by gaining access to cheaper goods produced under minimally-constrained use regimes. Their exit option (choosing alternatives, adopting welfare-certified products) is available but requires active choice against the property reading's default.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, consumers_animal_products, beneficiary,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__property_reading, property_owners_animal_use).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable legal framework for property ownership and use rights, reducing transaction costs for property transfers and use disputes by treating animals as chattels (moveable property) rather than beings with independent legal claims. Solves the coordination problem: 'Who may use this animal and for what purposes?' by defaulting to owner discretion, subject only to legislatively-specified welfare floors.
% TRANSFER_FUNCTION: Allocates unrestricted use and disposition rights from the legal system to property owners, and allocates welfare obligations narrowly (only where welfare statutes mandate them). The property reading transfers legal standing AWAY from animals and toward human owners.
% ABSENT_VOICES: Animals themselves have no voice by design — the property reading explicitly denies them independent moral or legal standing. Additionally, abolitionist constituencies (who deny the property premise itself) and radical welfare advocates (who would elevate animals' interests to constrain use structurally) are excluded from rule-setting; they can only lobby for narrow welfare carve-outs within a framework they reject.
% DISAPPEARANCE_RATIONALE: If the property reading disappeared overnight and were replaced by another reading (welfare or abolitionist), property law would reorganize: ownership claims might face legal challenges from animal interests, use practices would be constrained by recognized animal interests or rights, and the cost structure of animal-derived industries would shift dramatically. The property reading is what holds the current allocation in place.
% FOUNDING_PROBLEM: The need for a stable, predictable legal framework for property ownership and exchange. Animals are a category of property; the founding problem was how to treat them in law without creating endless disputes about use rights — the property reading solved this by denying animals independent standing and treating them as chattels like any other property.
% FOUNDING_PROBLEM_CORROBORATION: Property owners, industrial agriculture, and mainstream legal doctrine attest the founding problem is still live: they cite the need for certainty in use rights and the coordination function of treating animals as property. Courts and legislatures affirm this in rulings that deny standing to animals and treat use restrictions as voluntary welfare measures, not rights enforcement. Outside the benefiting parties: ethicists, animal advocates, and welfare-skeptical constituencies dispute whether the founding problem is genuinely 'live' or whether it was solved long ago and the property reading now persists as institutional inertia and rent-seeking. No corroboration from animals themselves is available under the property reading's core axiom.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is minimal (0.05) under the property reading because the axiom is that animals lack independent moral standing — if they have no interests as a matter of law, no one is structurally exploiting them. Suppression is low (0.12) because the constraint is maintained by legal codification and institutional consensus, not by coercive force against resistance from animals themselves (they have no legal capacity to resist). Theater is minimal (0.08) because the constraint performs its stated function: it allocates clear use rights and reduces transaction costs. Accessibility collapse is low (0.15) because the property reading faces active resistance from welfare and abolitionist constituencies — alternatives remain visible even if legally unavailable. The measurement series is nearly flat because the property reading is stable across the interval; it does not accumulate extraction or degrade in function. The small rises in suppression and theater reflect growing institutional effort to manage welfare statutes and contain abolitionist advocacy — not growth in the constraint itself, but growth in the effort required to defend it against competing readings.
 *
 * PERSPECTIVAL GAP:
 *   The property reading produces NO per-seat divergence by design: all seats that accept the property axiom (animals lack standing) compute the same constraint type and extractiveness. The divergence emerges between readings: a property-reading seat and a welfare-reading seat, evaluating the same institutional arrangement, will compute radically different types and ε values because they disagree on what counts as a victim. The engine should compute this as NO divergence within the property reading (consensus on the axiom → consensus on type) but sharp divergence ACROSS readings in the network (different axioms → different victim sets → different χ). This is the landmark cross-reading case: seats are aligned within a reading but opposed across readings.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no structural victim under the property reading by axiom — animals have no independent standing, so their interests do not factor into directionality. Property owners are beneficiaries (they retain unrestricted use rights). Consumers are beneficiaries (they gain cheaper access to animal products). Welfare and abolitionist advocates are excluded (their core premises are incompatible with the property axiom). The directionality is asymmetric in a different register: it is not that agents are exploited within the property frame, but that the property frame itself systematically forecloses certain readings and constituencies from being heard. This is a structural asymmetry that does not route through the standard victim-beneficiary mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is genuine: liberal legal systems needed a stable framework for property ownership and use rights. The property reading solved this by treating animals as chattels. The constraint persists because institutional structures (property law, industrial agriculture, market economics) are built on the property axiom. There is no mandatrophy signature here — the constraint's founding function (allocating use rights) is still live, even if other parties (welfare advocates, abolitionists) dispute whether that function justifies the axiom. The constraint's persistence is institutional lock-in and consensus within the benefiting parties, not zombie operation without function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_standing_ascription,
    'Is the denial of independent moral standing to animals a discovered fact about animal nature, or a normative choice made by legal systems for institutional convenience?',
    'Cross-cultural and historical analysis: do all legal systems deny animal standing, or only those with particular institutional structures (industrialized property regimes, Western liberal legal traditions)? Comparative analysis of jurisdictions that have granted limited standing to some animals (orangutans in some courts, rivers in New Zealand law) reveals the standing denial is contingent, not natural.',
    'If standing denial is discovered fact: the property reading is near-inevitable and the constraint has negligible extractiveness (ε ≈ 0.02). If standing denial is normative choice: the constraint is constructed and has moderate extractiveness (ε ≈ 0.35-0.45), and the welfare/abolitionist readings become live legal alternatives rather than ethical ideals. This is the core uncertainty routing toward omega rather than metric tuning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_standing_ascription, conceptual, 'Whether animal standing denial is discovered or constructed.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the property reading''s core axiom (animals lack independent moral standing) logically foreclose the abolitionist reading (animals are rights-holders), or do they coexist as competing framings of the same institutional space?',
    'Logical analysis: can one party hold both axioms consistently? Abolitionist logic says animals have intrinsic moral status that generates rights; property logic says animals lack moral standing. These are direct contradictions — foreclosure should apply. However, empirically, both readings coexist in the same jurisdictions, held by different parties and even by the same individual inconsistently (e.g., pet owners who deny welfare concerns about livestock). If coexistence is sustained despite logical tension, the relationship is ''coexists_with'' rather than ''forecloses'' — institutional pluralism over logical coherence.',
    'If foreclosure is the true relation: the readings cannot be simultaneously authoritative in one framework; one must be rejected. If coexistence is sustained: the constraint persists as one pole of an ongoing cultural contest without logical resolution. The type of relation (forecloses vs. coexists_with) changes how the engine models reading interference in the network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the property and abolitionist readings are logically foreclosed or institutionally coexistent.').

omega_variable(
    extraction_under_property_axiom,
    'Does the denial of animal standing itself constitute extraction (ε contribution), or is it merely the background axiom that zeroes extraction by construction?',
    'Examine what ''extraction'' means under the property reading: if extraction requires a victim to bear costs from the constraint, and the property reading denies animals can be victims, then extraction is definitionally impossible (ε → 0 by axiom). But if extraction is a structural property (resource transfer from a less-powerful to more-powerful agent) independent of the axiom, then the denial of standing is itself the mechanism that enables extraction by hiding it. The measurement hinges on whether extraction is axiom-relative or axiom-independent.',
    'If axiom-relative: ε ≈ 0.02-0.05 is correct; the constraint is Rope with minimal enforcement cost. If axiom-independent: ε ≈ 0.35-0.50; the constraint is Tangled Rope or Snare and the denial of standing is the suppression mechanism. This omega documents the measurement choice made in authoring ε = 0.05 under the property reading''s axiom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_under_property_axiom, preference, 'Whether extraction is axiom-relative or axiom-independent — the measurement choice made in authoring ε under the property reading.').

omega_variable(
    welfare_statute_boundary,
    'Are welfare statutes genuine constraints on the property reading, or are they secondary accommodations that preserve property rights at their core?',
    'Examine enforcement and exceptions: do welfare statutes operate as hard constraints that can override property claims, or do they set floors for ''acceptable treatment'' while preserving owner discretion above that floor? If owner intent (not animal suffering) determines statutory violation, statutes are accommodations; if animal suffering determines violation regardless of owner intent, they are genuine constraints.',
    'If statutes are genuine constraints: the property reading is already pluralized and the welfare reading coexists. If statutes are accommodations: the property reading is intact and welfare concerns are channeled as human obligations rather than animal rights. This affects whether welfare_reading is a sibling reading or a secondary layer within property_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_boundary, empirical, 'Whether welfare statutes are constraints on property or accommodations within it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t5, animal_status__property_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(anim_tr_t10, animal_status__property_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(anim_tr_t15, animal_status__property_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(anim_tr_t20, animal_status__property_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(anim_tr_t25, animal_status__property_reading, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(anim_be_t5, animal_status__property_reading, base_extractiveness, 5, 0.04).
narrative_ontology:measurement(anim_be_t10, animal_status__property_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(anim_be_t15, animal_status__property_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(anim_be_t20, animal_status__property_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(anim_be_t25, animal_status__property_reading, base_extractiveness, 25, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__property_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(anim_su_t5, animal_status__property_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(anim_su_t10, animal_status__property_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(anim_su_t15, animal_status__property_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(anim_su_t20, animal_status__property_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(anim_su_t25, animal_status__property_reading, suppression_requirement, 25, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__property_reading, 0.03).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three constraint stories, each instantiating a distinct reading with different axioms, victim sets, and ε values. The property_reading (this story) treats animals as objects without standing; the welfare_reading treats them as sentient beings with constrained-but-permissible use; the abolitionist_reading treats them as rights-holders precluding instrumental use. The readings are not perspectives on one constraint — they are different constraints grounded in different normative axioms. They share a kernel (the animal status question) but instantiate different answers. Network edges declare which readings are logically foreclosed, which coexist, and which influence each other. The property reading currently dominates institutionally; the welfare and abolitionist readings are challengers and coexist in the same jurisdictions as minority positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
