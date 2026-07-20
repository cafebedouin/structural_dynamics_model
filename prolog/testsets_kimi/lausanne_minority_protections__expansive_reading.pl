% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections â Expansive Institutional Reading
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint instantiates the expansive reading of the Lausanne Treaty
 *   minority protections (Articles 37â45), which holds that non-Muslim
 *   minorities in Turkey retain institutional self-administration, communal
 *   property rights, and clergy-formation capacity equivalent to pre-1923
 *   governance. The reading is one of three in a contested kernel: the
 *   restrictive reading limits protections to individual worship; the
 *   guarantor reading emphasizes international supervision; and this
 *   expansive reading maximizes substantive institutional autonomy. The
 *   constraint coordinates minority survival but extracts from Turkish
 *   sovereignty; it is claimed as rope because minority institutions are net
 *   beneficiaries and the arrangement solves a genuine collective-action
 *   problem (post-imperial institutional collapse), though the Turkish state
 *   experiences it as sovereignty cost. The claim/metric independence is
 *   maintained: the metrics acknowledge moderate extraction and rising
 *   theater as Turkish practice drifts from the reference frame, while the
 *   claim asserts genuine coordination.
 *
 * KEY AGENTS:
 *   - Minority religious institutions (beneficiary/organized/constrained) â receive protected autonomy
 *   - Turkish Republic (agenda-setter/powerful/constrained) â bears sovereignty cost and administers restriction
 *   - Guarantor states (observer/institutional/analytical) â irregular external supervision
 *   - ECtHR and human rights bodies (observer/institutional/analytical) â parallel jurisprudential pressure
 *   - Domestic secularist factions (excluded/moderate/constrained) â shape domestic resistance but excluded from treaty discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.3).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections â Expansive Institutional Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, 'e2859534-3b7d-46d6-97a3-489f61a373bd').
narrative_ontology:cs_kernel_codification('e2859534-3b7d-46d6-97a3-489f61a373bd', fixed_text).
narrative_ontology:cs_authority_grounding('e2859534-3b7d-46d6-97a3-489f61a373bd', lineage).
narrative_ontology:cs_interpretation_layer_present('e2859534-3b7d-46d6-97a3-489f61a373bd').
narrative_ontology:cs_reading_relation('e2859534-3b7d-46d6-97a3-489f61a373bd', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('e2859534-3b7d-46d6-97a3-489f61a373bd', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('e2859534-3b7d-46d6-97a3-489f61a373bd', foundational, institutional_autonomy_guaranteed).
narrative_ontology:cs_axiom_status(institutional_autonomy_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('e2859534-3b7d-46d6-97a3-489f61a373bd', institutional_autonomy_guaranteed, conventional).
narrative_ontology:cs_axiom('e2859534-3b7d-46d6-97a3-489f61a373bd', secondary, pre_1923_governance_continuity).
narrative_ontology:cs_axiom_status(pre_1923_governance_continuity, holdable).
narrative_ontology:cs_axiom_grounding('e2859534-3b7d-46d6-97a3-489f61a373bd', pre_1923_governance_continuity, conventional).
narrative_ontology:cs_reference_frame('e2859534-3b7d-46d6-97a3-489f61a373bd', minority_institutional_autonomy_framework).
narrative_ontology:cs_drift_state('e2859534-3b7d-46d6-97a3-489f61a373bd', contemporary_turkey, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2859534-3b7d-46d6-97a3-489f61a373bd', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_religious_institutions).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, institutional_continuity_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, pre_1923_millet_successor_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-Muslim religious communities in Turkeyâincluding the Greek Orthodox Ecumenical Patriarchate, Armenian Apostolic, Jewish, and Syriac communitiesâthat rely on Lausanne Treaty guarantees for legal personality, communal property ownership, internal self-administration, and the operation of theological schools. Their capacity to maintain pre-1923 institutional continuity depends entirely on the expansive reading of the treaty remaining interpretively viable against restrictive state practice.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_religious_institutions, beneficiary,
    organized, generational, constrained, national).

% Sovereign state administering the territory where minority institutions exist. It is treaty-bound to protect these institutions but systematically interprets Lausanne obligations as limited to individual worship, subjecting communal property, governance, and education to general domestic law. It bears the sovereignty cost of permitting extra-legal institutional autonomy within its territory and consistently resists expansive implementation through administrative and legislative restriction.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_republic, agenda_setter,
    powerful, generational, constrained, national).

% Signatory and guarantor statesânotably Greece and the United Kingdomâthat retain a diplomatic and legal interest in treaty compliance. They exercise supervision irregularly through bilateral pressure and international fora rather than through binding enforcement mechanisms, and their engagement has diminished over time.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, observer,
    institutional, generational, analytical, continental).

% European Court of Human Rights and Council of Europe bodies that adjudicate related minority-rights claims under the European Convention. They create parallel jurisprudential pressure that occasionally reinforces expansive protections, but they lack direct authority to interpret or enforce the Lausanne Treaty itself.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, ecrt_and_human_rights_bodies, observer,
    institutional, generational, analytical, continental).

% Turkish political and civil factions that view any communal religious autonomy as incompatible with unitary secular citizenship. They are structurally excluded from the international treaty-discourse that produces expansive readings, but they shape the domestic political climate that restricts implementation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, domestic_secularist_factions, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves functional continuity of non-Muslim religious minority institutions after the Ottoman millet system's collapse by guaranteeing legal personality, communal property, internal governance, and clergy formation through treaty-based protections.
% TRANSFER_FUNCTION: Transfers autonomous governance authority and immunity from general Turkish administrative law to minority religious institutions, without extracting monetary rents from them.
% ABSENT_VOICES: Turkish nationalist and secularist factions who oppose communal religious autonomy as incompatible with unitary state sovereignty; minority individuals who might prefer integration over institutional separateness; and closed or barred theological schools that cannot participate in the interpretive contest.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished, minority institutions would lose protected legal status and be absorbed into general Turkish property, education, and association law. Pre-1923 governance structures would likely dissolve or be expropriated, forcing a fundamental rearrangement of communal religious life in Turkey.
% FOUNDING_PROBLEM: The collapse of the Ottoman millet system and the creation of a Turkish nation-state left non-Muslim minorities vulnerable to loss of communal property, internal governance capacity, and religious education infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: International legal historians and minority-rights NGOs attest to ongoing institutional vulnerability. Turkish state institutions assert the problem is superseded by republican equality norms. European Court of Human Rights jurisprudence provides partial external corroboration that specific minority institutions remain at risk.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the constraint coordinates genuine institutional continuity without monetary rent extraction, though it structurally limits Turkish sovereignty. Suppression is moderate (0.30): the constraint itself is not coercive, but Turkish state practice suppresses expansive implementation. Theater ratio is 0.40 because Turkish state action increasingly involves performative gestures of protection (permitting symbolic presence while restricting substance) and minority institutions maintain governance facades despite functional erosion. Accessibility collapse is moderate (0.35): alternatives such as ECtHR litigation exist but are costly and indirect. Resistance is 0.55 because Turkish state institutions actively resist expansive interpretation while minority institutions and international bodies defend it.
 *
 * PERSPECTIVAL GAP:
 *   The minority institutional seat experiences the constraint as essential protective coordination without which communal life would collapse; the Turkish state seat experiences the same treaty text as an anachronistic infringement on unitary sovereignty. The guarantor state seat sees a diplomatic obligation it prefers not to enforce actively. These divergences are structurally predicted by the beneficiary/victim asymmetry and exit differentials.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious institutions are declared beneficiaries (low d, subsidized by the constraint). The Turkish Republic is not declared a beneficiary or victim; the directionality override for the powerful atom pushes its d toward the target end (0.75) to reflect the sovereignty cost it bears without capturing the structural relationship via the victim arrayâpreserving the rope claim while marking the seat asymmetry. Guarantor states and human rights bodies hold analytical exit and are treated as neutral observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâOttoman collapse threatening minority institutionsâis contested but not demonstrably dead, since the institutions remain vulnerable. Disappearance would cause world-rearrange, confirming that the constraint still supports live arrangements. The risk of mandatrophy is therefore moderate rather than acute: the constraint is not purely inertial, but the substantial practice drift and rising theater indicate that coordination is increasingly performed rather than fully functional. If the founding problem were ruled dead while the constraint still governed arrangements, a piton reclassification would be warranted; at present, the mismatch signal is insufficient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_individual_scope,
    'Does Lausanne Article 40 et seq. guarantee institutional religious autonomyâproperty, governance, and educationâor only individual worship and conscience?',
    'Comparative analysis of travaux prÃ©paratoires, subsequent state practice, and ECtHR parallel jurisprudence on Article 9 of the European Convention.',
    'Resolution would determine whether the expansive reading is textually supportable or a constructed expansion, which in turn would shift the kernel classification boundary between this reading and the restrictive sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_individual_scope, conceptual, 'Core textual ambiguity distinguishing expansive from restrictive readings').

omega_variable(
    enforcement_gap_vs_textual_ambiguity,
    'Is the gap between expansive reading and Turkish practice caused by absent enforcement mechanisms, or by genuine indeterminacy in the treaty text?',
    'Historical documentation of League of Nations and post-WWII supervisory practice paired with textual interpretation evidence.',
    'If enforcement gap, the constraint is a live rope lacking an enforcement scaffold; if textual ambiguity, the constraint itself is structurally contested and may be a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_vs_textual_ambiguity, empirical, 'Whether implementation failure is textual or enforcement-driven').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (post-Ottoman institutional collapse) been superseded by modern human rights frameworks, or does the Lausanne framework remain structurally necessary for these specific institutions?',
    'Comparative outcome analysis of minority institutions operating under ECtHR protection alone versus Lausanne-specific guarantees.',
    'If superseded, the constraint risks piton status despite active defense; if still necessary, it remains a live coordination rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, conceptual, 'Whether the coordination problem the treaty solved is still live').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_exp_tr_t0, lausanne_minority_protections__expansive_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lausanne_exp_tr_t20, lausanne_minority_protections__expansive_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(lausanne_exp_tr_t40, lausanne_minority_protections__expansive_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(lausanne_exp_tr_t60, lausanne_minority_protections__expansive_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(lausanne_exp_tr_t80, lausanne_minority_protections__expansive_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement(lausanne_exp_tr_t100, lausanne_minority_protections__expansive_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(lausanne_exp_be_t0, lausanne_minority_protections__expansive_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(lausanne_exp_be_t20, lausanne_minority_protections__expansive_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(lausanne_exp_be_t40, lausanne_minority_protections__expansive_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(lausanne_exp_be_t60, lausanne_minority_protections__expansive_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(lausanne_exp_be_t80, lausanne_minority_protections__expansive_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(lausanne_exp_be_t100, lausanne_minority_protections__expansive_reading, base_extractiveness, 100, 0.3).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(lausanne_minority_protections__expansive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
