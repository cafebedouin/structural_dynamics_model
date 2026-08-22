% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Medieval Latin as Organic Continuation of Classical Latin
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This constraint instantiates the continuity reading of the
 *   latin_correctness kernel: the claim that Medieval Latin is not a
 *   corruption of an earlier fixed standard but the same language undergoing
 *   the same kind of organic phonological, morphological, and lexical change
 *   any living language undergoes across centuries. Under this reading there
 *   is no victim set — medieval clerics, scribes, administrators, and
 *   scholastics are legitimate inheritors and continuators of the Latin
 *   tradition, not failed imitators of it. Extraction is low because the
 *   reading imposes no gatekeeping cost on medieval usage: it does not
 *   require medieval writers to pass a classical-conformity test to count as
 *   writing Latin. This is a distinct constraint from the rupture reading
 *   (which treats classical Latin as a fixed textual standard against which
 *   medieval usage is measured and found wanting) and the hybrid reading
 *   (which splits legitimacy by domain). Those are separate files, linked
 *   here via network edges, each with its own ε and stakeholder set — this
 *   file does not average across them.
 *
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
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Organic Continuation of Classical Latin").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "historical_linguistics/intellectual_history/philology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, 'aea0a4fc-7495-4cf2-9b13-74e793ca4587').
narrative_ontology:cs_kernel_codification('aea0a4fc-7495-4cf2-9b13-74e793ca4587', distributed).
narrative_ontology:cs_authority_grounding('aea0a4fc-7495-4cf2-9b13-74e793ca4587', practice).
narrative_ontology:cs_interpretation_layer_present('aea0a4fc-7495-4cf2-9b13-74e793ca4587').
narrative_ontology:cs_reading_relation('aea0a4fc-7495-4cf2-9b13-74e793ca4587', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('aea0a4fc-7495-4cf2-9b13-74e793ca4587', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('aea0a4fc-7495-4cf2-9b13-74e793ca4587', foundational, linguistic_change_is_continuity_not_corruption).
narrative_ontology:cs_axiom_status(linguistic_change_is_continuity_not_corruption, holdable).
narrative_ontology:cs_axiom_grounding('aea0a4fc-7495-4cf2-9b13-74e793ca4587', linguistic_change_is_continuity_not_corruption, empirically_contingent).
narrative_ontology:cs_axiom('aea0a4fc-7495-4cf2-9b13-74e793ca4587', foundational, medieval_users_are_legitimate_tradition_bearers).
narrative_ontology:cs_axiom_status(medieval_users_are_legitimate_tradition_bearers, holdable).
narrative_ontology:cs_axiom_grounding('aea0a4fc-7495-4cf2-9b13-74e793ca4587', medieval_users_are_legitimate_tradition_bearers, conventional).
narrative_ontology:cs_reference_frame('aea0a4fc-7495-4cf2-9b13-74e793ca4587', comparative_diachronic_linguistics_framework).
narrative_ontology:cs_drift_state('aea0a4fc-7495-4cf2-9b13-74e793ca4587', post_romance_philology_consolidation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aea0a4fc-7495-4cf2-9b13-74e793ca4587', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_clerics_and_scribes).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, vernacular_literate_administrators).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_universities).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, diachronic_linguists).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, language_change_is_continuous_not_ruptural).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, medieval_latin_users_are_legitimate_inheritors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wrote and copied texts in the Latin of their own era — inflected differently, drawing on an expanded vocabulary of theological and administrative terms, pronounced according to regional phonology. Under this reading their usage is not a falling-away from a standard but the same language doing what living languages do: their labor and authority as writers is treated as legitimate rather than as failed imitation.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_clerics_and_scribes, beneficiary,
    moderate, generational, mobile, continental).

% Used Latin for charters, law, and record-keeping without classical training, relying on forms that had drifted from Ciceronian norms. This reading validates their documents as legally and linguistically sound acts of the same tradition, rather than defective approximations requiring later correction.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, vernacular_literate_administrators, beneficiary,
    moderate, biographical, mobile, regional).

% Built scholastic Latin — a technical register full of coinages absent from classical sources — as the working medium of philosophy, medicine, and law. Under continuity framing this register is a natural extension of Latin's expressive capacity, not a corrupted derivative needing purification.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_universities, beneficiary,
    institutional, generational, mobile, continental).

% Study the attested sound changes, morphological simplifications, and lexical expansions across the classical-to-medieval interval as ordinary diachronic drift, comparable to any language's evolution. They neither gain nor lose from the reading but supply the comparative-linguistic evidence it rests on.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, diachronic_linguists, observer,
    analytical, civilizational, analytical, universal).

% Would object that this reading erases a real decline in fidelity to classical models and licenses centuries of usage they regard as barbarous. Their objection belongs to the rupture reading's constraint and is not part of this one — they are named here only as an absent voice this reading does not have to answer to.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, renaissance_humanist_critics, excluded,
    organized, generational, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single continuous linguistic tradition-membership standard: anyone writing Latin at any point between antiquity and the high medieval period counts as writing the same language, so texts, glosses, and later commentary can be read as one unbroken corpus rather than as two languages requiring separate grammars.
% TRANSFER_FUNCTION: Transfers legitimacy and interpretive standing from a narrow classical-only textual canon to the much larger body of medieval Latin production — scholastic philosophy, canon law, monastic chronicles, charters — without moving any material resource from an identifiable payer to an identifiable receiver.
% ABSENT_VOICES: Renaissance humanist critics and later classicizing purists, who hold the rupture reading, would object that this framing dissolves a meaningful standard of correctness; they are not silenced by this constraint but simply belong to a different reading's stakeholder set.
% DISAPPEARANCE_RATIONALE: If the continuity reading were abandoned entirely, the vast medieval Latin corpus would need re-classification as a derivative or corrupted register rather than a phase of the same language, and its authors would lose standing as legitimate participants in the Latin tradition rather than as its custodians — university curricula, philological method, and canon-law scholarship organized around treating medieval Latin as continuous Latin would all require re-founding.
% FOUNDING_PROBLEM: How to characterize continuous, attested linguistic change (phonological shift, morphological leveling, lexical expansion for new referents) across a millennium of unbroken written use without treating every generation's Latin as a failure to reproduce an earlier one.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical linguists working on Romance-language emergence — a field with no institutional stake in defending medieval Latin's prestige — independently model the classical-to-medieval transition using the same continuous-change apparatus applied to other attested language histories (e.g. Old English to Middle English), corroborating the continuity framing from outside the medievalist and clerical beneficiary set.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.12) and essentially flat across the interval because the continuity reading does not depend on suppressing medieval usage or extracting value from any party who bears a disproportionate cost — there is no toll collected on medieval Latin production. Suppression is low (0.08) since the reading imposes no active enforcement against alternative framings; it simply describes linguistic drift as it is comparatively studied. Theater ratio stays low and rises only slightly (0.05 to 0.10) reflecting increasing scholastic self-consciousness about technical vocabulary over the period, but not performative gatekeeping. Accessibility collapse and resistance are both low, consistent with a rope: this is a workable coordination frame with real alternatives (the sibling readings) that are not suppressed, and virtually no party actively resists it, since acceptance costs almost nothing structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval clerics, administrators, and universities are declared beneficiaries because the continuity reading confers legitimacy on their linguistic production without requiring them to pay any cost for the classification — their directionality sits near the full-beneficiary end. There is no victim group under this reading by the source material's own expected structural delta; declaring one would misrepresent the reading. Diachronic linguists sit at the analytical seat, observing rather than gaining or losing. Renaissance humanist critics are named as excluded (an absent voice) rather than as a victim, because their objection belongs structurally to the rupture reading's constraint, not to this one — including them as payers here would improperly import a rival reading's victim set into this ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — characterizing continuous attested change without treating every later generation's usage as failure — remains live rather than dead: the underlying linguistic fact pattern (drift, borrowing, register expansion) that motivated the continuity framing continues to describe every well-attested language history, so the reading's justification has not been overtaken by events. This distinguishes it from a mandatrophy case where a coordination frame outlives the problem it solved; here the coordination function (treating medieval and classical Latin as one continuous tradition for interpretive and pedagogical purposes) still tracks a live problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_framing_choice,
    'Is the classical-to-medieval Latin transition best modeled as continuous organic drift (this reading) or as departure from a fixed textual standard requiring correction (the rupture reading)? Both are coherent framings of the same attested textual record.',
    'This is not resolvable by additional textual data alone — the comparative-linguistic evidence (attested sound change, morphological leveling, lexical borrowing) is consistent with both framings; the difference is which framework treats the classical corpus as the reference point for judging correctness versus treating it as one historical stage among several. Resolution would require settling whether ''Latin'' names a language-stage or a language-lineage, a conceptual rather than empirical question.',
    'If the field converges on treating classical Latin as the sole reference standard (rupture reading), medieval Latin production is reclassified as corrupted or degenerate usage and the continuity reading''s low-extraction, no-victim structure would not apply to that reclassified constraint. If the field treats the lineage as continuous (this reading), no such reclassification occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_rupture_framing_choice, conceptual, 'Whether the kernel''s correct framing is lineage-continuity or standard-fidelity; the two framings are the continuity_reading and rupture_reading constraints respectively.').

omega_variable(
    hybrid_domain_split_boundary,
    'Does the hybrid reading''s domain split (classical norms for literary registers, medieval tolerance for technical registers) describe a real functional boundary in medieval practice, or is it a modern retrofit imposed on writers who did not themselves distinguish registers this way?',
    'Philological survey of whether medieval authors code-switched registers consciously (e.g. deliberately writing more classicizing Latin in poetry versus administrative prose) versus writing a single continuous register across genres.',
    'If medieval writers did register-switch deliberately, the hybrid reading has independent structural support and this continuity reading would need to be understood as the reading that applies specifically to the non-literary registers the hybrid reading also covers. If no such switching occurred, the hybrid reading''s domain split is an external imposition and this continuity reading''s scope is broader than the hybrid reading concedes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_domain_split_boundary, empirical, 'Whether the hybrid reading''s register-based domain split has independent evidentiary support in medieval usage.').

omega_variable(
    beneficiary_status_of_medieval_institutions,
    'Are medieval universities and clerical scriptoria genuine beneficiaries of the continuity reading, or does the reading simply describe a fact about language history that happens to be favorable to them without their having any stake in its adoption?',
    'Examine whether medieval institutions actively argued for or defended a continuity view of their own Latin (evidence of self-conscious linguistic apologetics) versus simply writing Latin without reflecting on its historical status at all.',
    'If medieval institutions did not argue for their own linguistic legitimacy, ''beneficiary'' status here is a retrospective analytic attribution rather than an active extraction-avoidance strategy, which would support treating this as a mountain-adjacent low-stakes description rather than an actively defended rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_medieval_institutions, conceptual, 'Whether declared beneficiaries actively benefit from or merely happen to align with the continuity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 200, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t200, latin_correctness__continuity_reading, theater_ratio, 200, 0.05).
narrative_ontology:measurement(lati_tr_t400, latin_correctness__continuity_reading, theater_ratio, 400, 0.06).
narrative_ontology:measurement(lati_tr_t700, latin_correctness__continuity_reading, theater_ratio, 700, 0.08).
narrative_ontology:measurement(lati_tr_t1000, latin_correctness__continuity_reading, theater_ratio, 1000, 0.09).
narrative_ontology:measurement(lati_tr_t1200, latin_correctness__continuity_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(lati_tr_t1300, latin_correctness__continuity_reading, theater_ratio, 1300, 0.1).

% Extraction over time
narrative_ontology:measurement(lati_be_t200, latin_correctness__continuity_reading, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(lati_be_t400, latin_correctness__continuity_reading, base_extractiveness, 400, 0.09).
narrative_ontology:measurement(lati_be_t700, latin_correctness__continuity_reading, base_extractiveness, 700, 0.1).
narrative_ontology:measurement(lati_be_t1000, latin_correctness__continuity_reading, base_extractiveness, 1000, 0.11).
narrative_ontology:measurement(lati_be_t1200, latin_correctness__continuity_reading, base_extractiveness, 1200, 0.12).
narrative_ontology:measurement(lati_be_t1300, latin_correctness__continuity_reading, base_extractiveness, 1300, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(latin_correctness__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint, latin_correctness__rupture_reading, and latin_correctness__hybrid_reading are three readings of a single contested kernel (latin_correctness) about whether medieval Latin usage constitutes legitimate continuation, corruption, or domain-split legitimacy relative to classical Latin. Each reading is authored as an independent constraint with its own ε: this reading is low-extraction with no victim set (medieval users as legitimate inheritors); the rupture reading is expected to carry substantially higher extractiveness and an explicit victim set (medieval users judged as corrupting a fixed standard); the hybrid reading splits the difference by register/domain. The three are linked via affects_constraints rather than merged, per the ε-invariance principle — averaging or parameterizing a single story by 'which reading' would violate DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
