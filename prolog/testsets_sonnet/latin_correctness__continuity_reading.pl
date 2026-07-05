% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Medieval Latin as Legitimate Organic Continuation of Classical Latin
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the latin_correctness
 *   kernel: medieval Latin represents legitimate organic linguistic evolution
 *   from classical Latin, analogous to how modern Romance languages evolved
 *   from Latin without thereby being 'corrupt' forms of it. Under this
 *   reading, phonological drift, morphological simplification, and vocabulary
 *   expansion in medieval usage are treated as ordinary features of a living
 *   language continuing to serve its speech community, not as deviations from
 *   a fixed textual ideal. This reading is one of three siblings sharing the
 *   latin_correctness kernel; the rupture_reading treats classical Latin as a
 *   fixed standard from which medieval usage corrupted, and the
 *   hybrid_reading splits the domain by register (classical norms for
 *   literary/rhetorical Latin, medieval norms tolerated for
 *   technical/practical Latin). This story generates ONLY the continuity
 *   reading as a clean, self-contained constraint with its own epsilon; the
 *   sibling readings are separate constraint files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - medieval_clerics_and_notaries: primary beneficiary of legitimation (moderate/constrained) — their everyday Latin usage is validated rather than judged deficient
 *   - monastic_and_cathedral_scriptoria: agenda-setter through continuous transmission practice (institutional/constrained)
 *   - vernacular_educated_clergy: beneficiary whose regionally-inflected Latin gains legitimacy (moderate/constrained)
 *   - modern_medievalist_philologists: analytical beneficiary whose discipline depends on this reading (analytical/analytical)
 *   - renaissance_humanist_tradition: excluded rival tradition whose corruption diagnosis is not adjudicative here (institutional/analytical)
 *   - comparative_historical_linguists: analytical observer supplying cross-linguistic corroboration (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.12).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.08).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Legitimate Organic Continuation of Classical Latin").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "historical_linguistics/intellectual_history/philology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, '3e4a4431-0937-4a54-b3c2-e9bd0c6914c3').
narrative_ontology:cs_kernel_codification('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', distributed).
narrative_ontology:cs_authority_grounding('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', practice).
narrative_ontology:cs_interpretation_layer_present('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3').
narrative_ontology:cs_reading_relation('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', foundational, living_usage_constitutes_linguistic_legitimacy).
narrative_ontology:cs_axiom_status(living_usage_constitutes_linguistic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', living_usage_constitutes_linguistic_legitimacy, conventional).
narrative_ontology:cs_axiom('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', secondary, no_privileged_synchronic_stage_of_a_language).
narrative_ontology:cs_axiom_status(no_privileged_synchronic_stage_of_a_language, holdable).
narrative_ontology:cs_axiom_grounding('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', no_privileged_synchronic_stage_of_a_language, empirically_contingent).
narrative_ontology:cs_reference_frame('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', classical_ciceronian_norm).
narrative_ontology:cs_drift_state('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', high_medieval_scholastic_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e4a4431-0937-4a54-b3c2-e9bd0c6914c3', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_clerics_and_notaries).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, monastic_and_cathedral_scriptoria).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, vernacular_educated_clergy).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, modern_medievalist_philologists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(latin_correctness__continuity_reading, vernacular_educated_clergy).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, language_change_is_not_corruption).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, living_usage_grounds_correctness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write charters, letters, and liturgical and administrative texts in the Latin they actually learned and use daily. Under this reading their usage counts as Latin proper, not as failed imitation of Cicero — they draw on an expanded vocabulary (feudal, ecclesiastical, technical terms) and simplified syntax without needing to justify departures from classical models.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_clerics_and_notaries, beneficiary,
    moderate, biographical, constrained, continental).

% Copy, teach, and transmit Latin texts across centuries, setting de facto norms through what they preserve, teach, and produce. Their continuous scribal practice is the mechanism of organic change this reading names as legitimate transmission rather than drift into error.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, monastic_and_cathedral_scriptoria, agenda_setter,
    institutional, generational, constrained, continental).

% Learn Latin as a living second language shaped by their vernacular phonology and grammar. This reading treats their accented, regionally inflected Latin as continuous with the classical language; they bear the ordinary cost of any language-learning but no penalty for departing from a fixed ancient standard, since no such standard is imposed on them.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, vernacular_educated_clergy, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(latin_correctness__continuity_reading, vernacular_educated_clergy, payer).

% Study medieval Latin texts on their own terms, as evidence of a living, evolving linguistic system rather than as degraded attempts at classical composition. This reading grounds their disciplinary legitimacy and grant funding for treating medieval corpora as primary objects of study rather than error catalogs.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, modern_medievalist_philologists, beneficiary,
    analytical, generational, analytical, global).

% Held that classical Latin was a fixed standard from which medieval usage had fallen away, and built an entire program of textual reconstruction and pedagogy on that premise. Under the continuity reading their diagnosis of corruption is treated as a historically situated polemic rather than a philological finding; they are not consulted as adjudicators within this reading's own frame.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, renaissance_humanist_tradition, excluded,
    institutional, civilizational, analytical, continental).

% Apply general models of language change (sound shift, analogical leveling, borrowing) to the classical-to-medieval Latin transition, treating it as a case among many rather than as a unique moral or aesthetic failure. They supply the comparative evidence this reading draws on without personally collecting from either reading's adoption.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, comparative_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent account of why medieval Latin texts should be read, taught, and studied as continuous with classical Latin rather than treated as failed or corrupted imitations — coordinating scribes, teachers, and scholars around a single evolving linguistic tradition rather than a fixed ancient target they perpetually fall short of.
% TRANSFER_FUNCTION: Moves interpretive legitimacy and disciplinary authority toward those who use, teach, and study Latin as it actually existed at any given historical moment, and away from a normative standard that would require every era's usage to be judged against classical texts.
% ABSENT_VOICES: The Renaissance humanist tradition and its intellectual descendants, who explicitly diagnosed medieval usage as corruption in need of correction, are structurally excluded from adjudicating within this reading's own frame — their objection is acknowledged as a historical position but not treated as evidence against the continuity claim.
% DISAPPEARANCE_RATIONALE: If the continuity reading disappeared, medieval Latin texts would need to be reclassified as deviations from a norm requiring correction; medievalist philology as a discipline treating medieval usage as primary evidence would lose its grounding premise, and pedagogy of medieval Latin would shift toward treating its distinctive features as errors to be flagged rather than data to be explained.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century philology needed a framework for treating the entire span of Latin usage from antiquity through the Middle Ages as a single object of linguistic study, rather than treating post-classical Latin as a debased imitation unworthy of serious grammatical description.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical linguists outside medieval studies corroborate the underlying claim by applying general models of regular sound change and morphological simplification to the Latin case exactly as they would to any other attested language continuum, providing evidence for continuity that is not sourced from medievalists' own disciplinary self-interest.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) and essentially flat across the interval because this reading describes an interpretive stance with no rent-collection mechanism attached to it — no toll is charged for using or studying Latin under this frame, and no gatekeeping fee is exacted from those who accept it. Suppression is low (0.08) because the continuity reading does not need to actively suppress alternatives to persist; it competes with the rupture reading in scholarly discourse but does not coerce compliance. Theater ratio is low and flat (0.1) because the actual philological and pedagogical work done under this reading (editing texts, teaching courses, publishing grammars of medieval Latin) is substantive rather than performative. Accessibility collapse is modest (0.2): once one accepts the general linguistic principle that living languages change without thereby becoming illegitimate, the rupture reading's alternative becomes harder to sustain on purely linguistic grounds, though it remains available as a normative/aesthetic stance. Resistance is low (0.15): the humanist tradition historically resisted this framing but that resistance is largely resolved within modern linguistics, persisting mainly in prescriptive pedagogical contexts outside historical linguistics proper.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval clerics, scriptoria, vernacular clergy, and modern medievalists all sit near the beneficiary end: the reading validates what they already do or study, without extracting anything from them in return. There is deliberately no victim group authored for this reading — the expected structural delta from the kernel context specifies exactly this: medieval users are treated as legitimate inheritors, not corruptors, so no coalition-of-the-extracted exists on this reading's own terms. The renaissance_humanist_tradition is excluded rather than victimized: their position is not accommodated within this reading's frame, but they are not coerced or extracted from by it, either — they simply operate under the rival rupture_reading.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy dynamic on this reading, since the founding problem (whether medieval Latin merits serious linguistic description as living language rather than a corpus of errors) remains live: the discipline of medieval Latin philology continues to exist, be funded, and produce scholarship precisely because this question is still contested in comparison with the rupture and hybrid readings. The absence of victims and the low extractiveness mean the classification is not at risk of masking an extraction dynamic behind a coordination story — this reading's coordination function (legitimating a field of study and its objects) is close to its entire content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_criterion,
    'Is there a principled linguistic criterion (as opposed to an aesthetic or institutional one) that distinguishes ''organic change'' from ''corruption,'' or is the distinction itself a normative overlay on a single underlying process of language change?',
    'Comparative analysis against other well-documented language continua (e.g., Latin-to-Romance vernaculars, Sanskrit-to-Prakrit) to determine whether structurally similar changes are labeled ''evolution'' in cases without institutional stakes and ''corruption'' in cases with them.',
    'If no principled linguistic criterion exists and the distinction tracks institutional interest rather than structural fact, the rupture reading''s normative framing would appear as a retrospectively motivated construction rather than a philological finding — strengthening the continuity reading''s claim to be the linguistically neutral description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_rupture_criterion, conceptual, 'Whether continuity/corruption is a linguistic fact or a normative overlay.').

omega_variable(
    beneficiary_circularity_risk,
    'Do the modern medievalist philologists who benefit from this reading''s legitimation of their object of study constitute independent corroboration, or does their professional stake in the continuity reading undermine the corroboration this story claims from comparative historical linguistics?',
    'Check whether comparative historical linguists working outside medieval studies (e.g., specialists in Romance historical phonology, general historical linguistics) independently apply the same regular-change models to Latin without reference to medievalist disciplinary interests.',
    'If the corroborating comparative linguists are themselves substantially drawn from or funded through medieval studies infrastructure, the founding_problem_corroboration claim weakens and the reading looks more like a beneficiary-internal consensus than an externally corroborated genealogy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_circularity_risk, empirical, 'Whether the claimed external corroboration is genuinely external to the beneficiary set.').

omega_variable(
    reading_selection_framing,
    'Is the choice to treat continuity_reading, rupture_reading, and hybrid_reading as three coequal readings of one kernel itself a framing choice, or does the register-based hybrid_reading actually describe most working philological practice, making the continuity/rupture split a simplification of a more granular reality?',
    'Survey of medieval Latin pedagogy and editorial practice across genres (charters vs. liturgy vs. poetry) to determine whether practitioners in fact apply different standards by register, which would support hybrid_reading as the empirically dominant practice rather than a genuine third position.',
    'If practice is predominantly register-differentiated, the pure continuity_reading and pure rupture_reading may both be idealized poles rarely held in practice, changing how much weight this story''s clean classification should carry relative to the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_framing, conceptual, 'Whether the three-reading kernel decomposition matches actual practice or oversimplifies it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(lati_tr_t20, latin_correctness__continuity_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__continuity_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(lati_tr_t60, latin_correctness__continuity_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(lati_tr_t80, latin_correctness__continuity_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(lati_tr_t100, latin_correctness__continuity_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(lati_be_t20, latin_correctness__continuity_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(lati_be_t40, latin_correctness__continuity_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(lati_be_t60, latin_correctness__continuity_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(lati_be_t80, latin_correctness__continuity_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(lati_be_t100, latin_correctness__continuity_reading, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(latin_correctness__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the correctness of medieval Latin' into structurally distinct claims per the epsilon-invariance principle. continuity_reading (this file) authors low extractiveness and no victim set. rupture_reading is expected to author a victim set (medieval scribes and their texts recast as sources of corruption to be corrected) and higher suppression (the humanist reconstruction program actively displaced medieval textual forms). hybrid_reading splits the domain by register and is expected to show intermediate values. All three share the kernel_id latin_correctness and are linked bidirectionally via affects_constraints; each carries its own cs_structure.reading_relations back to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
