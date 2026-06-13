% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Sentience and Regulated-Use Moral Framework
 *   domain: ethics/legal/animal_studies
 *
 * SUMMARY:
 *   This constraint instantiates the WELFARE READING of a contested kernel
 *   about animal moral status. The kernel is the claim that animals have
 *   moral relevance; three readings coexist in contemporary discourse. The
 *   welfare reading asserts that animal sentience generates an obligation to
 *   minimize suffering, but not an obligation to prohibit use. This reading
 *   dominates current regulatory frameworks in developed nations: industries
 *   use animals under welfare-certified standards, welfare organizations
 *   mediate between industries and the moral public, and use continues with
 *   public comfort. The structural consequence is asymmetric: animals are
 *   recognized as morally relevant but denied voice or veto in their own
 *   governance; industries benefit from permission-of-use; welfare
 *   organizations benefit from gatekeeping legitimacy; the abolitionist
 *   reading (which would deny use entirely) is excluded from standard-setting
 *   despite philosophical coherence.
 *
 * KEY AGENTS:
 *   - animal_welfare_organizations: Organized mediators with moderate institutional power; set welfare standards and certify compliance. Benefit from the permission-of-use framework (it sustains their role and funding). Directionality: low (beneficiary).
 *   - regulated_use_industries: Powerful institutional actors; continue use under welfare frameworks. Benefit from permission-to-use + moral legitimacy. Directionality: low (beneficiary with compliance cost).
 *   - animals_under_regulated_use: Powerless, trapped in use systems. Recognized as sentient but denied voice, veto, or exit. Directionality: high (target of constraint).
 *   - abolitionist_moral_community: Moderate power, constrained exit (excluded from governance). Hold alternative reading that would deny use entirely. Their exclusion is enforcement target. Directionality: high (excluded target).
 *   - regulatory_agencies: Institutional agenda-setters; codify welfare standards and delegate enforcement. Directionality: low-moderate (split role: set framework, constrain industries).
 *   - consumer_moral_frame: Collective beneficiary of moral permission; gain guilt-free use. Directionality: low (beneficiary of legitimacy).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.58).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.62).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Sentience and Regulated-Use Moral Framework").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "ethics/legal/animal_studies").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, 'ab9ea50a-ef81-42de-a2d8-9d4df8521cae').
narrative_ontology:cs_kernel_codification('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', distributed).
narrative_ontology:cs_authority_grounding('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', lineage).
narrative_ontology:cs_interpretation_layer_present('ab9ea50a-ef81-42de-a2d8-9d4df8521cae').
narrative_ontology:cs_reading_relation('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', foundational, animal_sentience_grounds_welfare_obligation).
narrative_ontology:cs_axiom_status(animal_sentience_grounds_welfare_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', animal_sentience_grounds_welfare_obligation, empirically_contingent).
narrative_ontology:cs_axiom('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', foundational, use_permissible_with_welfare_constraint).
narrative_ontology:cs_axiom_status(use_permissible_with_welfare_constraint, holdable).
narrative_ontology:cs_axiom_grounding('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', use_permissible_with_welfare_constraint, deontological).
narrative_ontology:cs_reference_frame('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', regulated_use_with_welfare_standards).
narrative_ontology:cs_drift_state('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', contemporary_post_industrial_agriculture, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab9ea50a-ef81-42de-a2d8-9d4df8521cae', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_use_industries).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_under_regulated_use).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, abolitionist_moral_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.42 and plateaus at 0.58 by time-point 25, indicating a constraint whose extraction stabilizes once welfare infrastructure matures. This trajectory reflects industries absorbing compliance costs and welfare organizations stabilizing their gatekeeping role. Theater_ratio rises from 0.35 to 0.48 (moderate plateau), indicating growing share of enforcement activity devoted to legitimacy-performance (certification, marketing humane credentials) relative to actual welfare improvement. Suppression is high and stable (0.55→0.62), reflecting active enforcement: regulatory agencies exclude abolitionist readings from governance, industries defend the use-permission framing against moral challenges, welfare organizations police the boundary between 'humane use' and abolition. Accessibility_collapse is moderate-high (0.65): once the framework is understood, individuals within it have constrained options — consume certified products, work in certification/industry, or exit to abolitionist framing (constrained, not trapped, because intellectual alternatives exist). Resistance is high (0.71): abolitionist movements, independent philosophers, and animal-advocacy groups actively contest the reading and its permission-of-use. The claimed type (tangled_rope) matches the structure: genuine coordination problem solved (ethical framework permitting use while addressing suffering), but with asymmetric extraction (animals pay through continued use; moral community pays through exclusion) and active enforcement (welfare organizations and agencies defend the reading against abolitionist alternatives).
 *
 * PERSPECTIVAL GAP:
 *   The welfare organizations and regulated industries experience this constraint as genuine coordination (solving a real problem: ethical use of sentient beings). Animals and abolitionist advocates experience it as enforced extraction (use is the problem, welfare framing legitimates it). Regulatory agencies experience split perspective: they coordinate legitimate standards but also exclude abolitionist alternatives and suppress that excluded voice. The engine computes per-seat types from these asymmetries: from the beneficiary seats, the constraint computes as tangled_rope (coordination + asymmetric extraction paid by powerless animals). From the animal and abolitionist seats, it computes as snare (the coordination story is cover; persistence depends on suppression of the use-prohibition alternative and powerlessness of the animals). This divergence is the measurement; do not resolve it toward a single type. The claim (tangled_rope) reflects the beneficiary-seat framing; the metrics describe beneficiary-seat experience; the engine's per-seat divergence reveals the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare organizations (agenda_setter, organized power) derive d near beneficiary end (~0.20): they set standards, collect legitimacy rents, and have mobile exit options (can shift to abolitionist framing or pure research). Regulated industries (beneficiary, powerful power) derive d near beneficiary end (~0.25): they collect permission-to-use and pass compliance costs to consumers; they have arbitrage exit (can exit to property_reading in different jurisdictions or violate welfare rules). Animals (payer, powerless) derive d at target end (~0.95): they are trapped in use systems, identity-locked to use (no animal can resign from being animal), and receive no compensation or voice. Abolitionist community (excluded, moderate power) derives d high (~0.80): they are excluded from governance despite having a live position, experience suppression of their reading, and have constrained exit (can exit to pure advocacy or jurisdictions with different readings, but not exit the moral landscape itself). Regulatory agencies split: as agenda-setters they have low d (~0.30) because they set the rules; as payers (they bear costs of enforcement capacity) they have moderate d (~0.55). No directionality overrides needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (industrial animal use is visibly cruel; public sentiment demands ethical frameworks) was live at the constraint's origin and has partially died: welfare improvements have reduced the most egregious harms (bare battery cages now prohibited in many jurisdictions). However, the constraint persists because its function has shifted from solving cruelty-reduction to legitimating continued use while appearing to address moral concern. The constraint does not exhibit the classic piton pathology (inert, mostly theatrical) because welfare enforcement remains functional: regulatory agencies conduct real inspections, compliance costs are real, welfare organizations provide genuine certification services. But the divergence between founding problem (reduce suffering) and current function (allow use with moral coherence) qualifies as partial mandatrophy. The theater_ratio rising from 0.35 to 0.48 captures this: increasing share of enforcement energy devoted to legitimacy theater rather than new welfare improvements. Mandatrophy_resolved would be false; the constraint has not fully resolved its original mission (suffering still occurs in regulated use) but has not wholly abandoned it either (welfare standards do prevent some harms). This is the complex case that mandatrophy was designed to identify: a constraint whose mandate has partly outlived its function but whose institutional role has solidified around a different function (legitimation rather than harm-reduction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_vs_rights_boundary,
    'Does acknowledging animal sentience logically entail a right not to be used, or is minimizing suffering within use structurally sufficient to satisfy the moral claim?',
    'Philosophical analysis of whether ''sentience grounds welfare obligations'' and ''sentience grounds use-prohibition rights'' are compatible in a single commitment framework; empirical observation of whether public moral intuition converges toward the welfare middle-ground or drifts toward abolitionist readings as use visibility increases.',
    'If sentience entails use-prohibition rights, this reading forecloses itself and should be reclassified as a property-reading (use-neutral) with false moral legitimation — extraction would reclassify as snare. If welfare obligations are sufficient to satisfy the moral claim, the reading is structurally coherent and remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_vs_rights_boundary, conceptual, 'Whether animal sentience grounds welfare obligations or use-prohibition rights (or both).').

omega_variable(
    welfare_certification_cover_story,
    'Do welfare improvements that occur under this framework represent genuine constraint on use-harms, or primarily provide moral legitimacy that enables continued use at higher social comfort?',
    'Empirical measurement of welfare outcomes (animal mortality, morbidity, behavioral indicators of suffering) under certification regimes versus unregulated use; parallel measurement of industry resource allocation (compliance spending vs. profit), consumer behavior (consumption patterns under certification vs. abolition pressure), and use volumes (does welfare certification expand or contract animal use).',
    'If welfare measures genuinely reduce suffering without increasing use volume, the constraint is a legitimate tangled_rope (coordination with extraction). If welfare measures increase use volume by removing moral objection (''I can buy guilt-free eggs''), the reading is primarily extractive of moral permission — theater_ratio would rise and ε would reclassify toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_certification_cover_story, empirical, 'Whether welfare certification constrains use or legitimates its expansion.').

omega_variable(
    powerless_voicelessness_asymmetry,
    'Can a morally relevant being (sentient animal) be party to a constraint that affects it while having zero voice in its governance? Does the stakeholder''s powerlessness invalidate the constraint''s legitimacy claim?',
    'Normative analysis of whether non-participation can be reconciled with moral status acknowledgment in a commitment-system framework; examination of whether other systems (e.g., child welfare, disability rights) have resolved the asymmetry through non-participatory representation mechanisms.',
    'If the asymmetry is indefensible, the constraint lacks the consent/voice structure it claims; the agenda-setter seat would compute as illegitimate and the constraint would reclassify toward snare (coercive protection of use, not coordination). If the asymmetry is reconcilable through representation or fiduciary duty, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(powerless_voicelessness_asymmetry, conceptual, 'Whether a powerless morally-relevant stakeholder can legitimately be bound by a constraint it does not participate in setting.').

omega_variable(
    sibling_reading_contest,
    'This reading coexists with property_reading and abolitionist_reading as live positions in public discourse. What conditions would cause one reading to foreclose another, and does that ever occur?',
    'Documentary evidence of jurisdictions or institutional bodies that have formally rejected one reading in favor of another; philosophical analysis of logical incompatibility; social-movement data on whether reading dominance shifts with generational or ideological change.',
    'If one reading historically forecloses another in practice (e.g., abolitionist reading gains jurisdiction and property reading is legally extinguished), the committer-axis kernel would be marked as contest-resolving rather than perpetually coexisting. The divergence between ''coexists_with'' relation and historical foreclosure would identify a false-stability assumption in the kernel model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contest, empirical, 'Whether kernel readings coexist permanently or one eventually forecloses the others in a given jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(anim_tr_t5, animal_moral_status__welfare_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__welfare_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement(anim_tr_t15, animal_moral_status__welfare_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__welfare_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(anim_tr_t25, animal_moral_status__welfare_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(anim_be_t5, animal_moral_status__welfare_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__welfare_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(anim_be_t15, animal_moral_status__welfare_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__welfare_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(anim_be_t25, animal_moral_status__welfare_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(anim_su_t5, animal_moral_status__welfare_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__welfare_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(anim_su_t15, animal_moral_status__welfare_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__welfare_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(anim_su_t25, animal_moral_status__welfare_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__welfare_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_moral_status__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (animal_moral_status). Three readings partition the answer-space: welfare_reading (this file) asserts sentience grounds welfare obligations but not use-prohibition; property_reading asserts sentience is morally irrelevant; abolitionist_reading asserts sentience grounds use-prohibition rights. Each reading instantiates a different constraint with different ε, different beneficiary/victim sets, different exclusions. The three stories are linked via network.affects_constraints to indicate kernel membership and cross-referencing dependency. Do not merge the readings into one story with measurement variance — the ε-invariance principle requires separate stories. Each reading is coherent as a standalone constraint; their coexistence as live positions in discourse is the kernel's structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__welfare_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
