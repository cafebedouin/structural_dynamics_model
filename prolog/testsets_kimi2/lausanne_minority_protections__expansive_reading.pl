% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections â Expansive Institutional Continuity Reading
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint story models the expansive reading of the Lausanne
 *   minority protections kernel. The 1923 Lausanne Treaty guarantees
 *   non-Muslim minorities in Turkey functional continuity of their pre-1923
 *   religious governance. This reading interprets those guarantees as
 *   encompassing institutional self-administration, communal property rights,
 *   and clergy formation through authorized theological schoolsânot merely
 *   individual worship. It therefore frames the treaty as a coordination
 *   mechanism that secures minority institutional survival against
 *   homogenizing state pressure. The kernel is contested: a restrictive
 *   sibling reading limits Lausanne to individual worship and subjects
 *   institutional matters to general Turkish law; a guarantor sibling reading
 *   relocates the primary enforcement mechanism to international diplomacy
 *   and European human rights litigation. This story instantiates only the
 *   expansive reading, authored as a clean, Îµ-invariant constraint.
 *
 * KEY AGENTS:
 *   - Non-Muslim minority institutions (Greek Orthodox, Armenian, Jewish communities): Beneficiaries of treaty-recognized legal personality and institutional autonomyâorganized power, constrained exit.
 *   - Turkish Republic: Agenda-setter administering the treaty framework; sovereign state bearing the sovereignty cost of recognized communal autonomyâinstitutional power, constrained exit by treaty and diplomatic cost.
 *   - Guarantor states (UK, France, etc.): Excluded under this reading; their supervisory role is backgrounded in favor of direct domestic institutional continuity.
 *   - European human rights bodies: Observers providing indirect jurisprudential pressure but not treated as the guarantee's primary source in this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.18).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.2).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections â Expansive Institutional Continuity Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '7d9c5cf8-1062-4c10-aae6-3867ac76e2f1').
narrative_ontology:cs_kernel_codification('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', fixed_text).
narrative_ontology:cs_authority_grounding('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', lineage).
narrative_ontology:cs_interpretation_layer_present('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1').
narrative_ontology:cs_reading_relation('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', foundational, institutional_autonomy_guaranteed).
narrative_ontology:cs_axiom_status(institutional_autonomy_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', institutional_autonomy_guaranteed, conventional).
narrative_ontology:cs_axiom('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', foundational, theological_education_right).
narrative_ontology:cs_axiom_status(theological_education_right, holdable).
narrative_ontology:cs_axiom_grounding('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', theological_education_right, conventional).
narrative_ontology:cs_reference_frame('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', pre_1923_institutional_continuity).
narrative_ontology:cs_drift_state('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', contemporary_turkish_republic, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7d9c5cf8-1062-4c10-aae6-3867ac76e2f1', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, non_muslim_minority_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate churches, religious foundations, and minority schools under Lausanne guarantees. Depend on treaty-recognized legal personality to hold communal property, administer internal affairs, and train clergy in theological seminaries. Their historical identity is tied to these specific institutions within Turkey; relocation would dissolve the communal continuity the treaty was designed to protect.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, non_muslim_minority_institutions, beneficiary,
    organized, generational, constrained, national).

% Sovereign state and successor to the Ottoman Empire, bound by Lausanne to recognize minority institutional autonomy. Administers the legal framework governing religious foundations, property registers, and school permits. Bears the sovereignty cost of permitting legally distinct communal governance within its territory. Has historically varied between accommodating and restricting minority institutional claims depending on domestic political cycles and international pressure.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_republic, agenda_setter,
    institutional, generational, constrained, national).

% Original signatories and diplomatic guarantors of the Lausanne Treaty who retain a latent interest in minority protection. Under this expansive domestic-institutional reading, their role is marginalized in favor of direct treaty-based minority-state relations. They would assert supervisory or diplomatic enforcement roles under the sibling guarantor reading but are largely absent from the everyday governance of minority institutions here.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, excluded,
    institutional, generational, mobile, global).

% Review individual complaints related to religious freedom and property rights through the ECtHR and related mechanisms. Their jurisprudence indirectly shapes Turkish legal practice, but this reading does not treat international litigation as the primary source of the institutional continuity guarantee; the treaty itself is held to directly secure the institutions.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_human_rights_bodies, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve non-Muslim minority religious institutions' functional continuityâself-administration, property holding, and clergy formationâagainst centralized nation-state homogenization that would otherwise dissolve distinct communal legal personalities into general administrative law.
% TRANSFER_FUNCTION: Moves internationally recognized legal guarantees and domestic administrative recognition from the Turkish state to minority institutions, securing their pre-1923 institutional capacities without assimilating them into the majority legal framework.
% ABSENT_VOICES: Lausanne guarantor states and European human rights enforcement bodies are sidelined as primary interpreters under this reading; Turkish nationalist actors who reject any special minority status, and secular universalist legal scholars who argue general human rights law suffices, are also absent from its legitimating framework.
% DISAPPEARANCE_RATIONALE: If the expansive guarantees vanished overnight, minority religious foundations would lose recognized legal personality and face property confiscation, theological seminaries would close for lack of authorized status, and community self-administration would collapse into direct state administration or informal operation without legal protection. The organizational form of these communities would reorganize around general association law or diaspora structures.
% FOUNDING_PROBLEM: The collapse of the Ottoman Empire and the establishment of the Turkish Republic threatened to erase the legal personality, property holdings, and educational autonomy of non-Muslim religious communities that had operated under the millet system.
% FOUNDING_PROBLEM_CORROBORATION: Lausanne Conference records and contemporary minority religious leaders outside the Turkish state apparatus attest to the ongoing need for treaty-specific institutional guarantees. Turkish state officials and universalist human rights scholars argue the problem has been superseded by the Turkish Constitution and the ECHR, contesting the need for distinct Lausanne-based institutional continuity.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.18, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.18) because the arrangement does not collect rents; it transfers legal guarantees to minority institutions without a capturing beneficiary. Suppression is low (0.20) because the constraint coordinates rather than coercesâalternatives such as general human rights litigation persist. Theater rises from 0.05 to 0.40 over the interval, reflecting growing nominal compliance alongside substantive restriction (foundation board seizures, seminary closures, bureaucratic permitting). Resistance sits at 0.40 because Turkish nationalist political factions consistently oppose special minority status. Accessibility collapse is moderate (0.30): general human rights law offers an alternative pathway, though it does not replicate the treaty-specific institutional guarantees.
 *
 * PERSPECTIVAL GAP:
 *   Minority institutions experience the constraint as protective coordination (low directionality, subsidy-like), while the Turkish state experiences it as a sovereignty cost (moderate directionality, near-symmetric). Nationalist factions within the state would experience it as an illegitimate externality (high directionality). The expansive reading itself is invisible to actors holding the restrictive reading, who see no coordination function at all. The engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Muslim minority institutions are declared beneficiaries, placing them near the full-beneficiary end (low d). The Turkish state is the agenda-setter; it does not collect from the constraint but incurs sovereignty costs, giving it a near-symmetric d. There are no declared victims, so no seat sits at the full-target end. Guarantor states are excluded from this reading's frame and carry no directional weight in its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâminority institutional survival after empireâis contested. If it were clearly dead, the constraint might degrade toward piton (theatrical maintenance of obsolete guarantees). However, ongoing property disputes, seminary closures, and community demographic pressure suggest the coordination problem remains live for the beneficiary institutions. The rising theater ratio signals performative drift but not yet functional atrophy. The contested founding-problem status prevents automatic mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansive_reading_kernel_location,
    'This constraint is the expansive reading of the Lausanne minority protections kernel; does the structural classification change if the restrictive reading (individual worship only) or guarantor reading (international enforcement) is adopted instead?',
    'Compare the three compiled constraint stories in the kernel family; the expansive reading''s rope classification depends on the presence of institutional beneficiaries and absence of victims, which the restrictive reading would collapse.',
    'If the restrictive reading is correct, the constraint has no coordination function and no beneficiaries, reclassifying toward mountain or piton; if the guarantor reading is correct, the coordination function is relocated to international enforcement, altering stakeholder directionalities and scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expansive_reading_kernel_location, conceptual, 'Structural dependence of classification on kernel reading choice').

omega_variable(
    treaty_obligation_sovereignty_cost,
    'Are the Lausanne institutional guarantees a permanently binding constraint on Turkish sovereignty, or a diplomatic concession that can be narrowed through domestic constitutional evolution?',
    'Analysis of treaty practice, subsequent agreement, and potential jus cogens status; comparison with other abrogated interwar minority treaty regimes.',
    'If binding and permanent, the constraint has higher accessibility collapse and lower exit for the Turkish state; if concessionary, the state has higher exit options and the constraint is closer to a degradable scaffold or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_obligation_sovereignty_cost, conceptual, 'Ambiguity of treaty obligation versus revocable concession').

omega_variable(
    founding_problem_current_relevance,
    'Does the problem of non-Muslim minority institutional survival in Turkey still require Lausanne-specific guarantees, or has it been resolved by general human rights law and domestic constitutional reform?',
    'Empirical assessment of minority foundation property disputes, seminary closures, community size trajectories, and the efficacy of ECHR and Turkish constitutional jurisprudence in replicating the treaty''s institutional protections.',
    'If the problem is dead, the constraint may be a piton (theatrical maintenance of obsolete guarantees); if live, it remains a functioning rope with genuine coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_current_relevance, empirical, 'Whether the founding problem persists or has been superseded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__expansive_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__expansive_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__expansive_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(laus_tr_t60, lausanne_minority_protections__expansive_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(laus_tr_t80, lausanne_minority_protections__expansive_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__expansive_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__expansive_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__expansive_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__expansive_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(laus_be_t60, lausanne_minority_protections__expansive_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(laus_be_t80, lausanne_minority_protections__expansive_reading, base_extractiveness, 80, 0.19).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__expansive_reading, base_extractiveness, 100, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(lausanne_minority_protections__expansive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
