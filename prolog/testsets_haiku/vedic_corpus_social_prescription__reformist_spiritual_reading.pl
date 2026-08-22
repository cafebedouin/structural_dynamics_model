% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Vedic Texts as Spiritual Cosmology (Reformist Reading)
 *   domain: religious_studies/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   The reformist spiritual reading of the Vedic corpus interprets ancient
 *   texts as guides to non-dual consciousness and metaphorical cosmology,
 *   with no prescriptive social content. Varna references are reread as
 *   descriptions of spiritual states (sattvic/rajasic/tamasic) or internal
 *   psychological principles, never as mandates for rigid social hierarchy.
 *   This reading is ONE instantiation within a contested kernel: the Vedic
 *   corpus itself. The same texts are read by orthodox interpreters as
 *   literally prescribing eternal Varna hierarchy, and were read by colonial
 *   administrators as a unified legal code to be extracted and codified for
 *   governance. The reformist reading is neither historically 'original' nor
 *   politically triumphant—it is a live hermeneutical position held by
 *   organized study communities and supported by scholarly philology, and it
 *   stands in structural relationship to the other readings. The constraint
 *   is NOT the texts themselves (which are fixed) but the reformist reading's
 *   coordination function: how study communities achieve coherent spiritual
 *   practice without endorsing hierarchy.
 *
 * KEY AGENTS:
 *   - Spiritual practitioners: individuals engaged in Vedic study and practice seeking non-hierarchical coherence
 *   - Vedic study communities (guru-shishya lineages): organized actors coordinating around reformist interpretation
 *   - Orthodox Varna adherents: excluded agents maintaining literalist reading
 *   - Colonial administrators (historical): excluded agents seeking legal codification
 *   - Secular Vedic scholars: observers providing textual and historical corroboration
 *   - Caste-oppressed communities: observers who may align with but do not drive the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Texts as Spiritual Cosmology (Reformist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, 'f509a15c-56e9-421a-ab4a-d3784c1c8093').
narrative_ontology:cs_kernel_codification('f509a15c-56e9-421a-ab4a-d3784c1c8093', fixed_text).
narrative_ontology:cs_authority_grounding('f509a15c-56e9-421a-ab4a-d3784c1c8093', lineage).
narrative_ontology:cs_interpretation_layer_present('f509a15c-56e9-421a-ab4a-d3784c1c8093').
narrative_ontology:cs_reading_relation('f509a15c-56e9-421a-ab4a-d3784c1c8093', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('f509a15c-56e9-421a-ab4a-d3784c1c8093', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('f509a15c-56e9-421a-ab4a-d3784c1c8093', foundational, vedic_varna_non_prescriptive).
narrative_ontology:cs_axiom_status(vedic_varna_non_prescriptive, holdable).
narrative_ontology:cs_axiom_grounding('f509a15c-56e9-421a-ab4a-d3784c1c8093', vedic_varna_non_prescriptive, empirically_contingent).
narrative_ontology:cs_axiom('f509a15c-56e9-421a-ab4a-d3784c1c8093', foundational, spiritual_equality_immanent).
narrative_ontology:cs_axiom_status(spiritual_equality_immanent, holdable).
narrative_ontology:cs_axiom_grounding('f509a15c-56e9-421a-ab4a-d3784c1c8093', spiritual_equality_immanent, deontological).
narrative_ontology:cs_reference_frame('f509a15c-56e9-421a-ab4a-d3784c1c8093', vedic_spiritual_cosmology_undifferentiated).
narrative_ontology:cs_drift_state('f509a15c-56e9-421a-ab4a-d3784c1c8093', contemporary_pluralist_academy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f509a15c-56e9-421a-ab4a-d3784c1c8093', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_study_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage with Vedic texts as guides to spiritual practice and direct experience of unity consciousness. Under this reading, Varna references are understood as spiritual principles (sattvic/rajasic/tamasic gunas) rather than rigid social prescriptions. They benefit from a cohesive interpretive framework that makes the texts personally applicable without demanding social hierarchy compliance.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Maintain living transmission lineages (guru-shishya parampara) centered on spiritual realization. They coordinate around shared interpretive principles: Varna is corruption (vikara) of original teachings, or is purely symbolic/internal, never a prescription for social hierarchy. They set the agenda for their own communities' study; they have chosen this reading as liberating and consonant with their experience.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_study_communities, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_study_communities, agenda_setter).

% Read Vedic texts as prescribing Varna hierarchy as eternal cosmic order. They are excluded from this reformist reading's coordination because the two readings fundamentally disagree on what the texts mean and what conclusions follow. Their objection is not to a material extraction but to the reading's rejection of Varna literalism.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_adherents, excluded,
    organized, generational, constrained, regional).

% Sought a unified, codifiable 'Hindu law' system extracted from texts and dharmashastra to serve colonial governance. They are excluded from the reformist reading, which rejects the premise that Vedic texts constitute legal prescriptions at all—spiritual and administrative framings are incommensurable in the reformist account.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_administrators, excluded,
    institutional, biographical, analytical, global).

% Conduct philological, historical, and philosophical analysis of Vedic texts from academic/secular standpoints. They analyze the textual evidence and note that literal Varna prescription is sparse in the Vedas proper (concentrated in later Dharmashastra); they provide external corroboration of the reading's textual foundation without necessarily endorsing the spiritual authority claims.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_scholars_secular, observer,
    organized, generational, analytical, global).

% Live under caste hierarchy instantiated partly through appeal to Vedic authority. From their position, the reformist reading offers one interpretive escape route (Varna is not prescribed, was corrupted, or is only symbolic), but they are not the reading's authors or primary beneficiaries—beneficiary status requires active participation in the study community. Scholarly corroboration of the reading's textual grounds may support their advocacy for Vedic reinterpretation, but that is downstream alignment, not the constraint's operation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, caste_oppressed_communities, observer,
    powerless, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a cohesive interpretive framework for Vedic study that permits spiritual practice without requiring social hierarchy endorsement. Solves the interpretive problem: 'How can sincere engagement with ancient texts proceed if those texts literally prescribe oppressive social order?' Answer: they don't—the social hierarchy is a later corruption or is purely metaphorical/internal.
% TRANSFER_FUNCTION: Transfers interpretive authority from literalist/hierarchical readings to reformist/spiritual readings within study communities. Those who adopt this reading gain coherence and spiritual legitimacy within their communities; those who maintain literalist readings are excluded from this particular coordination but retain authority in their own communities.
% ABSENT_VOICES: Caste-oppressed communities whose liberation movements could benefit from Vedic reinterpretation are not the primary agents—they are observers or indirect allies. Orthodox Varna adherents are excluded and would object that the reading distorts the texts' meaning. Colonial administrators are excluded and would object that spiritual reinterpretation undermines governable law extraction.
% DISAPPEARANCE_RATIONALE: If this reading disappeared (reverted entirely to literalist/orthodox readings), the constraint's coordination function would vanish for reformist communities, but global arrangements would not fundamentally rearrange—other readings would fill the space. Caste hierarchies would persist regardless (they are sustained by material enforcement, not textual interpretation alone). The contest is: does the reading matter for spiritual practice continuity, or is it merely one hermeneutical option among many? Reformist practitioners say yes (their practice depends on it); orthodox adherents say no (the literal reading is the only legitimate one).
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12 at interval end) because the reading imposes no material transfer, charges no fee, demands no exclusive allegiance. Beneficiaries gain interpretive coherence; the cost is low—intellectual work, community participation. No victim set exists; non-adherents are excluded, not harmed. Suppression is low (0.15) because the reading is maintained through discourse, scholarship, and voluntary community practice, not through coercion or legal enforcement. Theater is very low (0.08) because the coordination function is genuine (spiritual practice continuity) and not performative—the reading either makes sense of the texts or it doesn't; it cannot hide a fake function. Accessibility collapse is low (0.25): alternatives remain accessible (orthodox reading, secular reading, agnostic reading); the reformist reading does not monopolize interpretation. Resistance is moderate (0.42) from orthodox adherents and some conservatives who see the reading as distorting the texts; reformist communities experience low internal resistance. The measurement series models the reading's historical trajectory: extractiveness rose slightly after 1947 (Indian independence) as reformist interpretation became more institutionally visible and coordinated; theater fell as the reading matured from apologetic response to internal spiritual framework. Suppression requirement fell sharply after 1800 (post-colonial era) as the reading's advocates gained institutional platforms (universities, translation projects, international yoga/meditation movements) and coercive suppression became less feasible.
 *
 * PERSPECTIVAL GAP:
 *   From a reformist study community's perspective: this is authentic spiritual coordination, texts make sense, practice flows from understanding. From an orthodox Varna adherent's perspective: this is distortion and heretical reinterpretation of clear prescriptions. From a secular scholar's perspective: this is one defensible philological reading supported by textual analysis. From a caste-oppressed person's perspective: this reading might undermine hierarchy's authority, but it is not their constraint—they face material enforcement, not textual interpretation. The engine computes these gaps from stakeholder roles and power atoms: beneficiary seats (spiritual practitioners) derive low d; excluded seats (orthodox adherents) derive symmetric or slightly target d; observer seats (scholars) hold analytical d. No seat experiences extraction in the material sense.
 *
 * DIRECTIONALITY LOGIC:
 *   Spiritual practitioners and study communities are structural beneficiaries: they coordinate around the reading and gain coherence (d = 0.1–0.2, beneficiary end). Orthodox adherents experience the reading as a threat to their interpretive authority, not as a material extraction (d ≈ 0.5, symmetric or slightly target). Scholars are observers with no directionality (d = 0.5, analytical). Caste-oppressed communities are observers: they may benefit if the reading undermines hierarchy, but they are not the primary agents and face their own constraint (the material enforcement of caste, a separate and much more extractive constraint). The reformist reading does not extract from anyone; it opens interpretive space. The claim-metric gap is intentional: the reading is CLAIMED as rope (genuine coordination around spiritual practice) and the metrics confirm it—very low extraction, low suppression, low theater. The engine should classify this as rope from every seat where the reading is native (study communities), and may classify it differently for external observers (scholars as neutral, excluded agents as symmetric or fractionally target).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatropic. The founding problem (how to reconcile ancient hierarchy texts with contemporary spiritual equality aspiration) remains live. The reading persists because it solves that problem for its adherents, not because the problem is forgotten and the reading maintained theatrically. Theater ratio is very low (0.08) precisely because the function is real. A mandatrophy reading would have risen theater ratio and stable/rising extraction ratio despite claimed coordination—neither is present here. The reading is a genuine rope solving a genuine coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_evidence_for_spiritual_reading,
    'Do the Vedic texts themselves—at the level of philological analysis—support the reformist reading''s interpretation of Varna as spiritual/metaphorical rather than prescriptive?',
    'Detailed textual analysis comparing frequency and context of Varna references in the Vedas proper vs. later Dharmashastra; semantic study of varna terminology; comparison with orthogonal cosmological passages (Nasadiya Sukta, Purusha Sukta alternate readings).',
    'Strong textual support would establish the reading as defensible scholarship, weakening orthodox claims to literal interpretation and supporting the reformist reading''s authority. Weak textual support would reposition the reading as hermeneutical choice rather than textual constraint, potentially lowering its resistance and increasing its vulnerability to challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_evidence_for_spiritual_reading, empirical, 'Philological foundation for reinterpreting Varna as spiritual principle.').

omega_variable(
    kernel_vs_reading_boundary,
    'Is the distinction between (the Vedic texts as kernel) and (the reformist reading as one interpretation) stable, or does treating texts as inherently ambiguous already privilege the reformist reading over literalist readings?',
    'Meta-hermeneutical analysis of whether ''the texts are ambiguous'' is itself a reading-dependent claim or a reading-independent structural fact. Compare with the orthodox reading''s claim that ''the texts are clear.'' If orthogonal observers agree ambiguity exists, the boundary holds; if agreement breaks down, the distinction is reading-embedded.',
    'If the boundary is stable: each reading is a valid interpretation of a multivalent kernel, and the reformist reading is not privileged. If boundary is reading-dependent: the reformist reading embeds ambiguity-presumption in its framing, and that presumption itself distinguishes it from literalist readings (a deeper structural difference than content alone).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_boundary, conceptual, 'Whether kernel-reading boundary is independent of any reading''s hermeneutical stance.').

omega_variable(
    suppression_mechanism_history,
    'To what extent did the measured decline in suppression_requirement (from 0.40 at 1800 to 0.15 at 2026) reflect genuine institutional liberation (post-colonial independence, academic freedom, pluralization of authority) vs. the reading''s accommodation to state/institutional structures (the reading was never suppressed because it poses no threat)?',
    'Historical study of enforcement actions, institutional barriers, textual censorship, and lineage transmission during colonial and early post-colonial periods. If reformist teachers and texts were actively suppressed, suppression decline reflects liberation. If they were merely marginal, suppression decline reflects visibility without prior coercion.',
    'If genuine suppression-followed-by-liberation: the reading''s current stability is hard-won and depends on institutional protection. If never suppressed: the low suppression measures reflect the reading''s alignment with non-threatening interpretive space, not liberation, and suggests the reading may be vulnerable to challenge if institutional conditions shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_history, empirical, 'Whether low contemporary suppression reflects post-colonial liberation or long-standing marginality.').

omega_variable(
    identity_fusion_spiritual_commitment,
    'To what extent is adherence to the reformist reading identity-locked (practitioners have fused their spiritual identity with this interpretation) vs. mobile (practitioners could switch to other readings without losing spiritual coherence)?',
    'Ethnographic study of reformist communities: do teachers describe the reading as the only coherent interpretation, or as one defensible option? Do practitioners report that switching to an orthodox reading would destroy their practice? Do lineage transmission practices present alternatives or enforce conformity?',
    'High identity-lock would suggest the reading''s stability depends on psychological/relational commitment as much as textual evidence—making it vulnerable if practitioners encounter compelling counter-arguments. Low identity-lock would support the reading''s mobility and robustness: practitioners could maintain spiritual practice while acknowledging multiple valid interpretations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_spiritual_commitment, empirical, 'Degree of identity fusion binding practitioners to reformist reading specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1800, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(vedi_tr_t1920, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(vedi_tr_t1947, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1947, 0.12).
narrative_ontology:measurement(vedi_tr_t1980, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(vedi_tr_t2010, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(vedi_tr_t2026, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(vedi_be_t1920, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1920, 0.08).
narrative_ontology:measurement(vedi_be_t1947, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1947, 0.1).
narrative_ontology:measurement(vedi_be_t1980, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1980, 0.11).
narrative_ontology:measurement(vedi_be_t2010, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(vedi_be_t2026, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2026, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(vedi_su_t1920, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1920, 0.3).
narrative_ontology:measurement(vedi_su_t1947, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1947, 0.22).
narrative_ontology:measurement(vedi_su_t1980, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(vedi_su_t2010, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2010, 0.16).
narrative_ontology:measurement(vedi_su_t2026, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2026, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.06).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vedic_corpus_social_prescription kernel. The sibling readings (orthodox_varna_reading, colonial_orientalist_reading) are separate constraints with distinct ε values, distinct beneficiary/victim structures, and distinct claimed types. All three stories are linked via network.affects_constraints to form a constraint family. Do NOT conflate the three readings into a single story; the decomposition reflects the ε-invariance principle (OQ-26): different readings yield different ε values because they measure different referents' extractiveness. This reformist reading measures extractiveness of interpreting Vedas as spiritual cosmology; the orthodox reading measures extractiveness of Varna-literal reading; the colonial reading measures extractiveness of legalistic codification. Each story stands alone; the network links them as structural alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
