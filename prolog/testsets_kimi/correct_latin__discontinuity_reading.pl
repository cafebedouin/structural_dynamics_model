% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Classical Latin Textual Purity and Medieval Corruption Discontinuity
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the discontinuity reading of the
 *   correct_latin kernel: the claim that legitimate Latin is the Classical
 *   form recoverable from ancient texts, that medieval Latin represents a
 *   corrupt deviation from this standard, and that correct usage must be
 *   reconstructed through philological method rather than drawn from
 *   continuous medieval or living practice. Originating in Renaissance
 *   humanism and institutionalized in modern classical philology, the
 *   constraint coordinates scholarly access to antiquity while asymmetrically
 *   extracting status and resources from medievalists, living Latin users,
 *   and learners. It is one of three structurally distinct readings of the
 *   same kernel, linked to the continuity and hybrid readings via the
 *   constraint family network.
 *
 * KEY AGENTS:
 *   - Philological academy (agenda_setter/institutional/arbitrage): defines the reconstructed norm and certifies expertise.
 *   - Classical education sector (beneficiary/organized/constrained): markets and delivers the classical-only curriculum.
 *   - Medievalist scholars (payer/moderate/constrained): study stigmatized medieval forms and bear delegitimization costs.
 *   - Living Latin practitioners (payer/organized/identity_locked): bear normative pressure to abandon evolved usage.
 *   - Latin learners (payer/powerless/constrained): absorb the additional burden of the reconstructed register.
 *   - Descriptive linguists (observer/institutional/analytical): analyze the normative rupture as historical ideology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.71).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Classical Latin Textual Purity and Medieval Corruption Discontinuity").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '29f0a228-6dac-40a8-a212-0f71cabc9b14').
narrative_ontology:cs_kernel_codification('29f0a228-6dac-40a8-a212-0f71cabc9b14', fixed_text).
narrative_ontology:cs_authority_grounding('29f0a228-6dac-40a8-a212-0f71cabc9b14', lineage).
narrative_ontology:cs_interpretation_layer_present('29f0a228-6dac-40a8-a212-0f71cabc9b14').
narrative_ontology:cs_reading_relation('29f0a228-6dac-40a8-a212-0f71cabc9b14', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('29f0a228-6dac-40a8-a212-0f71cabc9b14', correct_latin__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('29f0a228-6dac-40a8-a212-0f71cabc9b14', foundational, classical_textual_purity_normative).
narrative_ontology:cs_axiom_status(classical_textual_purity_normative, holdable).
narrative_ontology:cs_axiom_grounding('29f0a228-6dac-40a8-a212-0f71cabc9b14', classical_textual_purity_normative, conventional).
narrative_ontology:cs_axiom('29f0a228-6dac-40a8-a212-0f71cabc9b14', foundational, medieval_latin_excluded).
narrative_ontology:cs_axiom_status(medieval_latin_excluded, holdable).
narrative_ontology:cs_axiom_grounding('29f0a228-6dac-40a8-a212-0f71cabc9b14', medieval_latin_excluded, conventional).
narrative_ontology:cs_reference_frame('29f0a228-6dac-40a8-a212-0f71cabc9b14', classical_textual_purity).
narrative_ontology:cs_drift_state('29f0a228-6dac-40a8-a212-0f71cabc9b14', medieval_latin_floruit, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('29f0a228-6dac-40a8-a212-0f71cabc9b14', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, philological_academy).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_education_sector).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medievalist_scholars).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, living_latin_practitioners).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, latin_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines correct Latin by reconstructing Classical usage from ancient texts; edits critical editions, sets scholarly standards, and certifies competence in classical philology. Derives institutional prestige, tenure lines, and editorial authority from its monopoly on the reconstructed norm.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, philological_academy, agenda_setter,
    institutional, generational, arbitrage, global).

% Operates curricula, examination boards, and publishing houses centered on the reconstructed classical register. Its market and cultural raison d'etre depend on the discontinuity claim that medieval forms are errors to be corrected rather than valid alternatives.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_education_sector, beneficiary,
    organized, generational, constrained, national).

% Study medieval Latin texts and textual communities whose language is classified as corrupt deviation within the dominant philological frame. Must continuously justify their object of study against the prestige standard of classical reconstruction.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medievalist_scholars, payer,
    moderate, biographical, constrained, global).

% Use Latin in liturgical, ceremonial, or conversational contexts where medieval and early modern evolution has shaped living usage. Their practice is deemed incorrect by the classical standard, creating pressure to abandon inherited forms for reconstructed ones.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, living_latin_practitioners, payer,
    organized, generational, identity_locked, global).

% Are taught a reconstructed extinct register as the sole correct form; medieval variants encountered in later reading are marked as mistakes, increasing the cognitive and temporal cost of acquisition.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, latin_learners, payer,
    powerless, biographical, constrained, national).

% The historical scribes, clerics, and authors who produced medieval Latin are absent from the normative conversation; they cannot contest the classification of their language as corruption.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_textual_communities, excluded,
    powerless, civilizational, trapped, continental).

% Study all historical forms of Latin descriptively; view the rupture narrative as an ideological construct of Renaissance humanism rather than a linguistic necessity. They observe the asymmetry without being bound to the normative frame.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, descriptive_linguists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, philological_academy).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving a standardized, historically anchored register that enables direct scholarly and educational access to ancient Roman literature, law, and philosophy across deep time.
% TRANSFER_FUNCTION: Moves authority over linguistic legitimacy and curricular prestige from medieval textual communities and descriptive usage to philological reconstructors and classical education institutions; moves status and resources toward critical editions and classical programs.
% ABSENT_VOICES: Medieval scribal and textual communities who produced the forms classified as corrupt are historically absent and cannot contest their delegitimization; vernacular-influenced Latin users and descriptive linguists are structurally marginalized in normative standard-setting bodies.
% DISAPPEARANCE_RATIONALE: If the discontinuity norm vanished, medieval Latin would gain equal curricular and ecclesiastical legitimacy, classical reconstruction would lose its gatekeeping function, scholarly hiring and publishing would redistribute toward medievalist and descriptive fields, and the authority of the philological academy to define correctness would dissipate.
% FOUNDING_PROBLEM: The perceived distance between medieval written Latin and the high literary registers of Cicero and Vergil threatened the Renaissance project of direct access to ancient literature and philosophy; scholars sought to purge 'barbaric' accretions and recover a pure classical instrument.
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists attest the problem is live, citing ongoing need for precise ancient registers. Medievalist historians and descriptive linguists outside the beneficiary set attest the founding problem is ideologically superseded; they corroborate that the corruption framing functions as a political-cultural project of the early modern period rather than an ongoing textual necessity.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint moves prestige, publication venues, and curricular resources toward classical reconstruction and away from medieval or living practice. Suppression (0.71) is higher still because the discontinuity claim requires active enforcement through peer review, editorial standards, and examination regimes to keep medieval forms outside the legitimate set. Theater_ratio (0.45) reflects that while textual criticism is a genuine scholarly practice, a significant share of activity involves performative correction of diachronic variation rather than recovery of lost information. Accessibility_collapse (0.68) is high: once the discontinuity frame is accepted, medieval Latin ceases to appear as a valid alternative standard. Resistance (0.48) is moderate because medievalists and linguists mount sustained critique but remain institutionally subordinate in classical-dominated venues. The metric series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (philological academy) experiences the constraint as necessary scholarly hygiene preserving access to antiquity; the payer seats (medievalists, living practitioners, learners) experience it as an arbitrary gate that extracts status and imposes costs on their legitimate practice. The engine computes this divergence from the structural asymmetry in exit options and beneficiary declarations rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The philological academy and classical education sector are structural beneficiaries: the constraint subsidizes their authority, market position, and identity. Their directionality sits near the beneficiary pole. Medievalist scholars, living Latin practitioners, and learners are structural targets: they bear the delegitimization, curriculum burden, and identity costs. Their directionality sits near the target pole, amplified by constrained or identity-locked exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemârecovering precise access to ancient textsâwas genuinely acute in the fourteenth century, but its resolution has long since shaded into institutionalized gatekeeping. The R5 genealogy flags this tension: founding_problem_status is contested, and disappearance_verdict is world_rearranges, indicating the arrangement persists beyond its original coordinating function. However, because concentrated beneficiaries (the academy and education sector) continue to capture substantial gains through active enforcement, the constraint reads as tangled_rope rather than piton: the coordination function (textual preservation) is real but fused with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discontinuity_kernel_framing,
    'Is the rupture between Classical and medieval Latin an empirical linguistic fact or a normative philological construct?',
    'Comparative sociolinguistic and textual-community analysis: determine whether the boundary is discoverable in usage patterns or only in humanist polemics.',
    'If the rupture is purely normative, the constraint''s implicit claim to naturality collapses, reinforcing its classification as tangled_rope or worse; if empirical, the coordination function gains stronger justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_kernel_framing, conceptual, 'Whether the Classical/medieval boundary is empirical or ideological.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of medieval Latin structural (curricular and editorial gatekeeping) or internalized (scholars believing classical Latin is intrinsically superior)?',
    'Track citation patterns, hiring decisions, and curriculum composition after institutional reforms that formally equalize medieval and classical registers.',
    'If internalized, effective suppression exceeds the structural metric, increasing computed extraction for victim seats even after external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of medieval Latin legitimacy.').

omega_variable(
    textual_authority_vs_natural_law,
    'Does the constraint covertly claim the status of a natural law (languages naturally decay) or is it openly a conventional standard?',
    'Genealogical analysis of the corruption narrative in humanist rhetoric to detect naturalization tropes.',
    'Natural-law framing would trigger false-summit evaluation toward mountain; conventional framing keeps the classification in the constructed regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_vs_natural_law, conceptual, 'Whether the constraint is presented as natural or conventional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t100, correct_latin__discontinuity_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(corr_tr_t200, correct_latin__discontinuity_reading, theater_ratio, 200, 0.4).
narrative_ontology:measurement(corr_tr_t300, correct_latin__discontinuity_reading, theater_ratio, 300, 0.5).
narrative_ontology:measurement(corr_tr_t400, correct_latin__discontinuity_reading, theater_ratio, 400, 0.55).
narrative_ontology:measurement(corr_tr_t500, correct_latin__discontinuity_reading, theater_ratio, 500, 0.5).
narrative_ontology:measurement(corr_tr_t600, correct_latin__discontinuity_reading, theater_ratio, 600, 0.48).
narrative_ontology:measurement(corr_tr_t700, correct_latin__discontinuity_reading, theater_ratio, 700, 0.45).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(corr_be_t100, correct_latin__discontinuity_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(corr_be_t200, correct_latin__discontinuity_reading, base_extractiveness, 200, 0.65).
narrative_ontology:measurement(corr_be_t300, correct_latin__discontinuity_reading, base_extractiveness, 300, 0.72).
narrative_ontology:measurement(corr_be_t400, correct_latin__discontinuity_reading, base_extractiveness, 400, 0.75).
narrative_ontology:measurement(corr_be_t500, correct_latin__discontinuity_reading, base_extractiveness, 500, 0.7).
narrative_ontology:measurement(corr_be_t600, correct_latin__discontinuity_reading, base_extractiveness, 600, 0.65).
narrative_ontology:measurement(corr_be_t700, correct_latin__discontinuity_reading, base_extractiveness, 700, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(corr_su_t100, correct_latin__discontinuity_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement(corr_su_t200, correct_latin__discontinuity_reading, suppression_requirement, 200, 0.75).
narrative_ontology:measurement(corr_su_t300, correct_latin__discontinuity_reading, suppression_requirement, 300, 0.8).
narrative_ontology:measurement(corr_su_t400, correct_latin__discontinuity_reading, suppression_requirement, 400, 0.82).
narrative_ontology:measurement(corr_su_t500, correct_latin__discontinuity_reading, suppression_requirement, 500, 0.8).
narrative_ontology:measurement(corr_su_t600, correct_latin__discontinuity_reading, suppression_requirement, 600, 0.75).
narrative_ontology:measurement(corr_su_t700, correct_latin__discontinuity_reading, suppression_requirement, 700, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the correct_latin kernel, specifically the discontinuity reading. It is structurally distinct from the continuity and hybrid readings, which instantiate different constraints with different beneficiary/victim structures, epsilon profiles, and directionality derivations. The epsilon-invariance principle requires separate stories for each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
