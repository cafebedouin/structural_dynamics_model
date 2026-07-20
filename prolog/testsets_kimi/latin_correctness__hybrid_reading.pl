% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Hybrid Latin Correctness Regime (Literary/Technical Bifurcation)
 *   domain: historical/linguistic/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_reading of the latin_correctness
 *   kernel: the claim that classical Latin norms govern literary and
 *   rhetorical domains while medieval Latin forms retain legitimacy only in
 *   technical and practical contexts. Emerging in Renaissance humanism and
 *   consolidating through early modern educational institutions, this
 *   bifurcation created a status hierarchy in which high-prestige discourse
 *   required classical purity, while practical communication was grudgingly
 *   permitted to retain post-classical forms. The arrangement coordinates a
 *   pan-European learned identity around a purified classical standard, but
 *   extracts status and enforceable deference from technical writers who are
 *   pressured to emulate an unattainable classical ideal for domains where
 *   medieval forms would serve more effectively. The claim/metric
 *   independence is maintained: the constraint is claimed as necessary
 *   differentiation (tangled_rope) while the metrics describe moderate but
 *   structurally real extraction.
 *
 * KEY AGENTS:
 *   - humanist_scholars: Agenda-setter (institutional/analytical) â adjudicates legitimacy boundaries and captures institutional prestige
 *   - literary_elite: Beneficiary (powerful/mobile) â cultural capital preserved by bifurcation
 *   - technical_writers: Payer (moderate/constrained) â pressured toward unattainable classical standards
 *   - medieval_defenders: Excluded (moderate/constrained) â silenced voice defending medieval literary legitimacy
 *   - modern_philologists: Observer (analytical/global) â retrospective analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.55).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.45).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Hybrid Latin Correctness Regime (Literary/Technical Bifurcation)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical/linguistic/intellectual_history").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, 'b53eb77c-cb52-4d97-a640-dd0e5db5de2c').
narrative_ontology:cs_kernel_codification('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', fixed_text).
narrative_ontology:cs_authority_grounding('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', lineage).
narrative_ontology:cs_interpretation_layer_present('b53eb77c-cb52-4d97-a640-dd0e5db5de2c').
narrative_ontology:cs_reading_relation('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', foundational, classical_literary_supremacy).
narrative_ontology:cs_axiom_status(classical_literary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', classical_literary_supremacy, deontological).
narrative_ontology:cs_axiom('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', foundational, technical_pragmatism_waiver).
narrative_ontology:cs_axiom_status(technical_pragmatism_waiver, holdable).
narrative_ontology:cs_axiom_grounding('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', technical_pragmatism_waiver, instrumental).
narrative_ontology:cs_reference_frame('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', classical_rhetorical_supremacy).
narrative_ontology:cs_drift_state('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', early_modern_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b53eb77c-cb52-4d97-a640-dd0e5db5de2c', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, literary_elite).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_writers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate the boundary between legitimate classical and medieval Latin usage across literary and technical domains. Control access to university posts, patronage, and publication channels. Their institutional authority and social prestige depend on maintaining classical standards as the supreme register for high-status discourse.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_scholars, agenda_setter,
    institutional, generational, analytical, continental).

% Churchmen, statesmen, and poets who write in the high literary register. The bifurcation guarantees their classical Latin exclusive prestige, distinguishing their discourse from mere technical or practical communication and preserving their cultural capital across the Republic of Letters.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, literary_elite, beneficiary,
    powerful, biographical, mobile, continental).

% Physicians, jurists, merchants, and artisans who produce practical Latin texts. They face sustained pressure to humanize their prose toward classical norms to gain legitimacy, even when medieval syntactic and lexical forms would communicate technical content more clearly and economically.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_writers, payer,
    moderate, biographical, constrained, regional).

% Scholastic and monastic writers who regard medieval Latin as a legitimate literary and theological language in its own right. Their voice is structurally excluded from the legitimacy discourse; the hybrid reading grants them only a technical ghetto, not parity in literary domains.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medieval_defenders, excluded,
    moderate, biographical, constrained, continental).

% Later historical and linguistic scholars who analyze the hybrid regime retrospectively. They trace the bifurcation to specific humanist ideological commitments rather than to necessary linguistic function, identifying the power asymmetry embedded in the standard.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, modern_philologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, humanist_scholars).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified, high-prestige pan-European learned register for diplomatic, ecclesiastical, and literary communication, preventing fragmentation of the Republic of Letters into mutually unintelligible vernaculars or regionally divergent Latins.
% TRANSFER_FUNCTION: Transfers cultural prestige and communicative legitimacy from technical and practical writers to humanist scholars and literary elites by bifurcating the language into a classical high register and a medieval low register.
% ABSENT_VOICES: Defenders of medieval Latin as a fully legitimate literary language, and technical practitioners who would prefer to write in emergent vernaculars rather than aspiring to an ever-receding classical purity that does not serve their domains.
% DISAPPEARANCE_RATIONALE: If the bifurcated legitimacy regime vanished, technical writers would no longer face pressure to emulate unattainable classical standards; the literary register would lose its exclusive prestige marker; educational institutions would reorganize around either full classical reconstruction (rupture) or full medieval continuity; and the social hierarchy of the Republic of Letters would flatten.
% FOUNDING_PROBLEM: The perceived barbarization of Latin in the later Middle Ages and the need for a restored prestige standard capable of unifying high-status discourse across politically fragmented Europe.
% FOUNDING_PROBLEM_CORROBORATION: No external corroboration exists. The founding narrative is asserted exclusively by humanist scholars who benefit from the status hierarchy. Technical writers and later empirical philologists attest that medieval Latin already solved the communicative problem and that the hybrid regime imposes an artificial aesthetic hierarchy rather than repairing a genuine communicative breakdown.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the bifurcation does not extract wealth directly but rather status and communicative efficiency; technical writers must either absorb the cost of classical emulation or suffer diminished legitimacy. Suppression (0.45) is moderate because enforcement is primarily pedagogical and social rather than legal or violent, though institutional gatekeeping (patronage, publication, university appointment) provides real teeth. Theater ratio (0.30) reflects that much humanist activity was genuine scholarly recovery, but a substantial portion served to perform classical purity for status distinction. Accessibility collapse (0.58) is moderate-high: once the hierarchy is understood, medieval alternatives in literary domains effectively collapse as live options. Resistance (0.42) reflects persistent scholastic defense and eventual vernacular challenge. Measurements show extraction intensifying as humanist institutions consolidate (c. 1500-1700), then slightly declining as vernaculars rise.
 *
 * PERSPECTIVAL GAP:
 *   The humanist scholar seat experiences the constraint as genuine coordination: it solves the problem of pan-European learned communication by restoring a purified standard. The technical writer seat experiences the same structure as extraction: their practical competence is devalued and they bear the cost of stylistic aspiration that does not serve their communicative goals. The modern philologist seat sees the bifurcation as historically contingent ideology. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars are declared beneficiaries and agenda-setters: they administer the classical standard and their institutional authority derives from it (d near beneficiary end). Literary elites are beneficiaries: the bifurcation preserves their cultural capital (d low). Technical writers are declared victims: they bear the cost of the status hierarchy and have constrained exit (d near target end). Medieval defenders are excluded rather than directly targeted, situating them outside the primary extraction circuit but confirming the suppression of alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mislabeling by requiring both coordination and extraction. A pure rope would show no victim set; a pure snare would show no genuine coordination function. The technical writers are partial victims because the constraint does not suppress medieval forms in technical domains entirely, but it devalues them and pressures convergence toward the classical standard. If the coordination function (literary standard) were separable from the extraction (technical devaluation), the constraint would decompose into two distinct constraints per the epsilon-invariance principle. Their entanglement in a single bifurcated legitimacy regime is what makes this a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_bifurcation,
    'Is the literary/technical bifurcation a natural functional differentiation of a language, or an artificial status hierarchy imposed by humanist ideology?',
    'Comparative historical sociolinguistics: compare the hybrid regime against other diglossic situations to determine whether the domains map to genuine functional differentiation or to status-marking.',
    'If purely functional, the constraint reclassifies toward rope with low extraction; if artificial status hierarchy, tangled_rope classification holds and victim set is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_bifurcation, conceptual, 'Whether the literary/technical split is natural or ideological.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the pressure on technical writers structural (institutional exclusion from patronage, publication barriers, university gatekeeping) or internalized (self-censorship toward classical norms due to prestige aspiration)?',
    'Archival study of technical writers'' manuscripts, prefaces, and correspondence for evidence of self-censorship vs. external editorial or patronage demands.',
    'If internalized, effective extraction exceeds structural measures; if purely structural, the constraint operates more like an enforcement mechanism with clear gatekeepers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in literary prestige hierarchy.').

omega_variable(
    kernel_reading_position,
    'Does the hybrid reading of latin_correctness operate as a materially enforced constraint, or merely as an interpretive preference without institutional teeth?',
    'Historical analysis of educational curricula, publication practices, and patronage networks to establish whether non-compliance with the hybrid regime incurred material penalties.',
    'If purely discursive, extractiveness and suppression should be revised downward toward rope or piton; if institutionally enforced, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, empirical, 'Whether the hybrid reading had institutional enforcement or was mere aesthetic preference.').

omega_variable(
    sibling_reading_influence,
    'How would the structural classification change if the continuity or rupture reading were adopted instead of the hybrid reading?',
    'Construct parallel constraint stories for continuity_reading and rupture_reading and compare computed classifications across the kernel family.',
    'Continuity would likely eliminate victims (rope/mountain); rupture would likely expand victims (snare). This story''s partial victim set is specific to the hybrid reading''s bifurcated structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_influence, conceptual, 'Structural delta between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(lati_tr_t80, latin_correctness__hybrid_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(lati_tr_t160, latin_correctness__hybrid_reading, theater_ratio, 160, 0.3).
narrative_ontology:measurement(lati_tr_t240, latin_correctness__hybrid_reading, theater_ratio, 240, 0.34).
narrative_ontology:measurement(lati_tr_t320, latin_correctness__hybrid_reading, theater_ratio, 320, 0.32).
narrative_ontology:measurement(lati_tr_t400, latin_correctness__hybrid_reading, theater_ratio, 400, 0.3).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lati_be_t80, latin_correctness__hybrid_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement(lati_be_t160, latin_correctness__hybrid_reading, base_extractiveness, 160, 0.54).
narrative_ontology:measurement(lati_be_t240, latin_correctness__hybrid_reading, base_extractiveness, 240, 0.58).
narrative_ontology:measurement(lati_be_t320, latin_correctness__hybrid_reading, base_extractiveness, 320, 0.56).
narrative_ontology:measurement(lati_be_t400, latin_correctness__hybrid_reading, base_extractiveness, 400, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lati_su_t80, latin_correctness__hybrid_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement(lati_su_t160, latin_correctness__hybrid_reading, suppression_requirement, 160, 0.46).
narrative_ontology:measurement(lati_su_t240, latin_correctness__hybrid_reading, suppression_requirement, 240, 0.5).
narrative_ontology:measurement(lati_su_t320, latin_correctness__hybrid_reading, suppression_requirement, 320, 0.48).
narrative_ontology:measurement(lati_su_t400, latin_correctness__hybrid_reading, suppression_requirement, 400, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the latin_correctness kernel, which decomposes into three structurally distinct claims per the epsilon-invariance principle: continuity (medieval Latin as organic evolution), hybrid (bifurcated legitimacy), and rupture (classical as fixed standard, medieval as corruption). Each reading has a different epsilon, beneficiary structure, and victim set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
