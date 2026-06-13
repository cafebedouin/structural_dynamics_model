% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__young_earth_literal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__young_earth_literal, []).

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
 *   constraint_id: genesis_creation_cosmology__young_earth_literal
 *   human_readable: Genesis Young Earth Literal Creation Days Cosmology
 *   domain: religious/theological/epistemic
 *
 * SUMMARY:
 *   The young-earth literal reading of Genesis asserts that the six days of
 *   creation in Genesis 1-2 describe literal 24-hour solar days occurring
 *   approximately 6000-10000 years ago (commonly dated via Ussher's
 *   chronology or later extensions). Young-earth creationist institutions
 *   (evangelical churches, fundamentalist colleges, organizations like
 *   Answers in Genesis) teach and enforce this reading as Scripture's literal
 *   truth, incompatible with evolutionary cosmology and deep-time geology.
 *   The constraint extracts by transferring epistemic authority from
 *   empirical methodology to textual interpretation and suppresses
 *   evolutionary biology, geology, and theistic evolution readings through
 *   institutional authority claims and public-education battles. The
 *   structure is tangled-rope: a coordination function (unified answer to
 *   origin questions for adherents) is fused with asymmetric extraction
 *   (scientists and educators bear suppression costs; theistic evolutionists
 *   are excluded from 'authentic' Christian discourse; secular educators face
 *   curricular constraints). Active enforcement is required: creationist
 *   school boards, textbook battles, pulpit authority, institutional identity
 *   policing.
 *
 * KEY AGENTS:
 *   - young_earth_creationist_institutions: Agenda-setters (organized power, identity-locked time horizon, set enforcement apparatus for textual interpretation)
 *   - evolutionary_biologists & earth_scientists: Payers (powerful actors globally, but suppressed in local educational contexts, mobile exit nationally)
 *   - secular_educators: Payers (moderate power, constrained exit, embedded in communities, forced to suppress or covertly teach)
 *   - theistic_evolutionists: Dual payer/excluded (bear reputational suppression within creationist communities, excluded from conversations about 'authentic' Christianity)
 *   - young_earth_laity: Beneficiaries (identity-locked, receive coherent cosmology from institutions)
 *   - state_educational_authorities: Observers (adjudicate curricular conflicts, witness all parties)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__young_earth_literal, 0.68).
domain_priors:suppression_score(genesis_creation_cosmology__young_earth_literal, 0.76).
domain_priors:theater_ratio(genesis_creation_cosmology__young_earth_literal, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, extractiveness, 0.68).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__young_earth_literal, tangled_rope).
narrative_ontology:human_readable(genesis_creation_cosmology__young_earth_literal, "Genesis Young Earth Literal Creation Days Cosmology").
narrative_ontology:topic_domain(genesis_creation_cosmology__young_earth_literal, "religious/theological/epistemic").

domain_priors:requires_active_enforcement(genesis_creation_cosmology__young_earth_literal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__young_earth_literal, 'ca7ba825-f9d7-44d2-8465-1673a1f58ad7').
narrative_ontology:cs_kernel_codification('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', fixed_text).
narrative_ontology:cs_authority_grounding('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', lineage).
narrative_ontology:cs_interpretation_layer_present('ca7ba825-f9d7-44d2-8465-1673a1f58ad7').
narrative_ontology:cs_reading_relation('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', genesis_creation_cosmology__literary_framework, forecloses).
narrative_ontology:cs_reading_relation('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', genesis_creation_cosmology__theistic_evolution, coexists_with).
narrative_ontology:cs_axiom('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', foundational, genesis_literal_temporal_sequence).
narrative_ontology:cs_axiom_status(genesis_literal_temporal_sequence, holdable).
narrative_ontology:cs_axiom_grounding('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', genesis_literal_temporal_sequence, empirically_contingent).
narrative_ontology:cs_axiom('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', foundational, scriptural_literalism_primacy).
narrative_ontology:cs_axiom_status(scriptural_literalism_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', scriptural_literalism_primacy, deontological).
narrative_ontology:cs_reference_frame('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', literal_genesis_cosmology_framework).
narrative_ontology:cs_drift_state('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', contemporary_empirical_falsification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ca7ba825-f9d7-44d2-8465-1673a1f58ad7', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, young_earth_creationist_institutions).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, scriptural_literalist_doctrine).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, evolutionary_biologists).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, earth_scientists).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, secular_educators).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, theistic_evolutionists).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__young_earth_literal, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_cosmology__young_earth_literal, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__young_earth_literal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_cosmology__young_earth_literal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_cosmology__young_earth_literal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 at interval end, rising from 0.52 at t0, because the young-earth constraint systematically subordinates empirical method to textual authority — a net transfer of epistemic legitimacy from scientists to religious institutions. The rise is steady through t20 (point of maximum institutional enforcement circa 2000s schoolboard battles), then plateaus after t25 as state adoption of anti-evolution curriculum peaks and scientific consensus hardens. Suppression is high (0.76) and rises from 0.58 at t0, because enforcement machinery (schoolboard policies, curriculum review, pulpit authority, institutional identity policing) must actively block alternatives — evolutionary pedagogy doesn't persist by passive neglect. Theater ratio rises from 0.18 to 0.42, indicating growing performative maintenance: defensive creationist responses (YEC institute publications, textbook disclaimers, media campaigns) consume institutional resources without changing the core empirical contradiction. The measurements are on one shared time grid (all metrics at all 9 time points) to avoid misalignment artifacts. The theater rise reflects the constraint's structural position: its core claim is empirically falsifiable (young earth is falsified by radiometric data, geological stratigraphy, and light travel time), so persistence increasingly depends on performative defense rather than empirical viability.
 *
 * PERSPECTIVAL GAP:
 *   The young-earth creationist institutions and laity experience the constraint as necessary truth-preservation (their seat d ≈ 0.0–0.2, beneficiary side): Scripture's authority is what's at stake, and the young-earth reading appears to them as the plain, unavoidable meaning. From the evolutionary biologists' and earth scientists' seat (d ≈ 0.9–1.0, target side), the same constraint is experienced as suppression masquerading as truth: their empirical methodology is dismissed and their research is subordinated to textual authority claims. From theistic evolutionists' seat (d ≈ 0.7–0.8), the constraint operates as exclusion: they are labeled theologically compromised precisely because they hold a reading of Scripture that is empirically compatible with evolutionary cosmology. The engine should compute these differently because the structural beneficiary/victim configuration creates asymmetric directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (young_earth_creationist_institutions, young_earth_laity) derive d near 0.0–0.2: they set the constraint, enforce it, and collect institutional benefits (authority, identity coherence, pedagogical control). Victims (evolutionary_biologists, earth_scientists, secular_educators) derive d near 0.8–1.0: they bear suppression costs, endure epistemic subordination, and face constrained professional options in regions where creationist institutions dominate. Theistic evolutionists are victims (d ≈ 0.7–0.8) because they bear reputational suppression and exclusion despite holding Scripture-based readings; they are not beneficiaries. The constraint's victims are not those who reject Scripture (secular scientists) but those who engage Scripture seriously and reach different conclusions — the suppression extends to alternative hermeneutics, not just to science. This prevents the constraint from hiding behind 'we just defend Scripture against secular attack'; it actively suppresses religious alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: early Protestant and Fundamentalist movements faced a real crisis of scriptural authority under modernist and critical scholarship. A literal chronological reading of Genesis provided a text-based answer that didn't require surrendering to German higher criticism. By the early 20th century, the problem was substantially solved by evangelical scholarship itself: scholars like E.J. Young, John Collins, and later Peter Enns demonstrated that Genesis deploys ANE literary forms and that rigorous biblical scholarship (even believing scholarship) doesn't require factual-historical claims about cosmological age. The founding problem is now dead (evangelical scholars overwhelmingly read Genesis 1-2 as literary; genealogies are recognized as selective; theistic evolution is taught in major evangelical institutions). Yet the young-earth constraint persists through institutional inertia and identity-lock enforcement: creationist institutions have made young-earth literalism a boundary marker of orthodoxy, so reformulating would require painful institutional identity revision. The constraint's persistence without its founding justification satisfies mandatrophy conditions: the problem it was meant to solve has been resolved by its own tradition (rigorous evangelical biblical scholarship), yet the constraint remains enforced. This is the core signal of institutional extraction riding on atrophied coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genealogical_chronology_truncation,
    'Are the genealogies of Genesis 5 and 11 chronologically complete and mathematically exact, or are they selective and schematic as demonstrated by internal Biblical evidence (repeated names, parallel structures, comparable length to ANE genealogical forms)?',
    'Careful exegetical analysis of genealogical structure across Scripture (especially comparing Genesis genealogies to Chronicles, which omits names; analysis of genealogical patterns in ANE literature; examination of whether genealogies in Scripture are demonstrably selective elsewhere — e.g., Matthew''s genealogy of Jesus skips generations).',
    'If genealogies are demonstrably selective/schematic, the chronological inference (6000-10000 years) rests on a category mistake (applying mathematical precision to literary forms). The constraint''s empirical foundation dissolves; the question becomes hermeneutical, not scientific, and theistic evolutionists and literary readings move from ''unbelieving'' to ''alternative legitimate interpretation.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genealogical_chronology_truncation, empirical, 'Whether genealogical chronologies support the young-earth timeline or are literary/selective forms like those elsewhere in Scripture.').

omega_variable(
    textual_authority_vs_empirical_method,
    'What is the epistemological relationship between textual authority (Scripture interpreted literally) and empirical evidence (radioactive dating, geological stratigraphy, fossil sequences)? Is one a constraint on the other, or are they incommensurable frameworks?',
    'Philosophical and theological analysis of how the constraint treats contradictions: Does it adjust auxiliary hypotheses infinitely (radiometric dating is unreliable, fossil sequences are God''s layers, etc.), or does it appeal to an authority principle (God''s Word ranks higher than human interpretation of rocks)? If infinite adjustment, the constraint is epistemically unfalsifiable and operates as dogma; if authority principle, the constraint is coherent but requires accepting that empirical method is subordinate to textual interpretation in this domain.',
    'If infinite adjustment, the constraint''s persistence is explicable only through institutional enforcement, not rational commitment. If authority principle, the constraint is coherent to holders but requires abandoning scientific methodology''s epistemic authority in cosmology/biology. Either resolution strengthens the tangled-rope / snare reading (extraction via suppression of alternative epistemologies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_vs_empirical_method, conceptual, 'Whether the constraint operates through empirical adjustment (falsifiable) or epistemological authority (unfalsifiable by design).').

omega_variable(
    identity_lock_reversibility,
    'For young-earth laity and creationist institutions, how much of the identity lock is irreversible theological/spiritual fusion versus identity constructed through institutional reinforcement? Would exit become more viable if theistic evolution were presented as equally Christian?',
    'Longitudinal study of defectors from young-earth communities: What cognitive/identity shifts preceded leaving? Do defectors report relief or spiritual loss? Ethnographic study of young-earth communities whose denominations shift to theistic evolution (e.g., some evangelical colleges): Do members defect or reformulate?',
    'If the identity lock is substantially institutional (reversible via new reference groups), the constraint''s persistence depends on continuous enforcement by creationist institutions; weakening institutional enforcement would reduce suppression. If lock is irreversible (deep spiritual fusion), suppression will persist even after institutional enforcement weakens, because targets carry the internalized constraint with them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-locking to young-earth cosmology is reversible institutional construction or irreversible spiritual identification.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is this constraint a genuine alternative reading of Genesis cosmology grounded in plausible Scripture interpretation, or does it instantiate a false-summit problem: young-earth literalism appears natural/inevitable (''the plain reading'') but benefits identifiable institutional actors and suppresses alternative readings that are equally grounded in Scripture?',
    'Survey of biblical scholars across denominational lines: What proportion of evangelical scholars (non-liberal, believing scholars) interpret Genesis 1-2 as using ANE literary forms rather than modern scientific history? If majority, young-earth literalism is a *chosen* reading, not a transparent reading, and its naturality claim collapses.',
    'If young-earth literalism is one reading among peer alternatives (literary framework, theistic evolution), the constraint is honest about its status as a doctrinal choice backed by institutional enforcement. If it claims to be the plain/necessary reading while institutional surveys show it is a minority among believing scholars, the constraint operates as false-summit: appearing natural while resting on institutional power and definition-control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, empirical, 'Whether young-earth literalism is a natural/unavoidable reading or an institutional choice falsely presenting as inevitable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__young_earth_literal, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_cosmology__young_earth_literal, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gene_tr_t5, genesis_creation_cosmology__young_earth_literal, theater_ratio, 5, 0.22).
narrative_ontology:measurement(gene_tr_t10, genesis_creation_cosmology__young_earth_literal, theater_ratio, 10, 0.26).
narrative_ontology:measurement(gene_tr_t15, genesis_creation_cosmology__young_earth_literal, theater_ratio, 15, 0.3).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_cosmology__young_earth_literal, theater_ratio, 20, 0.35).
narrative_ontology:measurement(gene_tr_t25, genesis_creation_cosmology__young_earth_literal, theater_ratio, 25, 0.39).
narrative_ontology:measurement(gene_tr_t30, genesis_creation_cosmology__young_earth_literal, theater_ratio, 30, 0.41).
narrative_ontology:measurement(gene_tr_t35, genesis_creation_cosmology__young_earth_literal, theater_ratio, 35, 0.42).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_cosmology__young_earth_literal, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gene_be_t5, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(gene_be_t10, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(gene_be_t15, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(gene_be_t20, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(gene_be_t25, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(gene_be_t30, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(gene_be_t35, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(gene_be_t40, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(gene_su_t5, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(gene_su_t10, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(gene_su_t15, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(gene_su_t20, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(gene_su_t25, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(gene_su_t30, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(gene_su_t35, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(gene_su_t40, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__young_earth_literal, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(genesis_creation_cosmology__young_earth_literal, 0.18).
narrative_ontology:affects_constraint(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology__literary_framework).
narrative_ontology:affects_constraint(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology__theistic_evolution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the three-member genesis_creation_cosmology kernel family. The three readings (young_earth_literal, theistic_evolution, literary_framework) share the same fixed text (Genesis 1-2) and the same authority claim ('Genesis answers origin') but diverge on what the text is claiming and how it constrains cosmological belief. Young-earth literal is the most extractive reading because it uniquely suppresses empirical methodology and excludes alternative Christian hermeneutics. Theistic evolution and literary-framework readings permit evolutionary science; young-earth forbids it. The three stories must be linked via network edges because they are readings of the same kernel — the upstream reading (young_earth_literal, institutionally enforced and empirically contested) influences both downstream readings (theistic_evolution, literary_framework, which define themselves partly in response to young-earth claims). See constraint stories genesis_creation_cosmology__theistic_evolution and genesis_creation_cosmology__literary_framework for sibling analyses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
