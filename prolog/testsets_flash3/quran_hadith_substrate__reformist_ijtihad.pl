% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad: Contextual Interpretation Prioritizing Quranic Ethics
 *   domain: islamic_jurisprudence/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the 'reformist ijtihad' reading of the
 *   Quran-Hadith substrate in Islamic jurisprudence. It mandates contextual
 *   interpretation when classical rulings conflict with contemporary ethics,
 *   human rights, or public interest (maslaha), prioritizing the Quran's
 *   ethical trajectory over literalist hadith application. This reading aims
 *   to make Islamic law more adaptable and just, benefiting progressive
 *   Muslims and marginalized groups, but it challenges traditional authority
 *   structures. The claimed type is 'rope' because it offers a coordination
 *   mechanism for navigating modernity while maintaining Islamic principles,
 *   with moderate extractiveness and relatively low suppression compared to
 *   more rigid readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.45).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.3).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad: Contextual Interpretation Prioritizing Quranic Ethics").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "islamic_jurisprudence/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '661f04b2-fa68-4075-9e0b-0aa251e88fd2').
narrative_ontology:cs_kernel_codification('661f04b2-fa68-4075-9e0b-0aa251e88fd2', fixed_text).
narrative_ontology:cs_authority_grounding('661f04b2-fa68-4075-9e0b-0aa251e88fd2', expertise).
narrative_ontology:cs_interpretation_layer_present('661f04b2-fa68-4075-9e0b-0aa251e88fd2').
narrative_ontology:cs_reading_relation('661f04b2-fa68-4075-9e0b-0aa251e88fd2', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('661f04b2-fa68-4075-9e0b-0aa251e88fd2', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('661f04b2-fa68-4075-9e0b-0aa251e88fd2', foundational, quranic_ethical_trajectory_supremacy).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('661f04b2-fa68-4075-9e0b-0aa251e88fd2', quranic_ethical_trajectory_supremacy, deontological).
narrative_ontology:cs_axiom('661f04b2-fa68-4075-9e0b-0aa251e88fd2', foundational, maslaha_as_interpretive_principle).
narrative_ontology:cs_axiom_status(maslaha_as_interpretive_principle, holdable).
narrative_ontology:cs_axiom_grounding('661f04b2-fa68-4075-9e0b-0aa251e88fd2', maslaha_as_interpretive_principle, conventional).
narrative_ontology:cs_reference_frame('661f04b2-fa68-4075-9e0b-0aa251e88fd2', early_islamic_ijtihad_spirit).
narrative_ontology:cs_drift_state('661f04b2-fa68-4075-9e0b-0aa251e88fd2', contemporary_global_ethics_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('661f04b2-fa68-4075-9e0b-0aa251e88fd2', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_lgbtq_minorities).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, human_rights_advocates).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_authority_structures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from interpretations that align Islamic teachings with contemporary ethical values and human rights, allowing for a more inclusive and adaptable practice of faith. They actively advocate for this interpretive methodology.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, generational, mobile, global).

% Experience greater inclusion and justice within Islamic frameworks when interpretations prioritize ethical outcomes and public interest over rigid classical rulings. This reading offers them greater agency and protection.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_lgbtq_minorities, beneficiary,
    powerless, biographical, constrained, global).

% Find common ground and potential for collaboration with Islamic legal thought when it embraces universal human rights norms and ethical considerations, fostering interfaith dialogue and legal reform.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Bear the cost of diminished interpretive monopoly and challenged legitimacy, as their reliance on strict adherence to classical fiqh and literalist hadith application is questioned. This reading undermines their established authority.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_authority_structures, payer,
    institutional, civilizational, identity_locked, global).

% Observe the internal dynamics of Islamic legal reform, which can influence the integration of Islamic personal law within secular legal frameworks or inform policy on religious freedom and minority rights.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the application of Islamic law by providing a methodology for reconciling classical rulings with contemporary ethical challenges, ensuring the faith remains relevant and just in diverse modern contexts.
% TRANSFER_FUNCTION: Transfers interpretive authority from rigid adherence to classical precedent towards a dynamic engagement with Quranic ethical principles and public interest, shifting influence from traditional scholars to those capable of contextual ijtihad.
% ABSENT_VOICES: Ultra-traditionalist literalists who reject any form of contextual ijtihad that deviates from established classical interpretations are largely excluded from the discourse that validates this reading, as their methodology is deemed incompatible with its foundational premises.
% DISAPPEARANCE_RATIONALE: If this interpretive methodology vanished, Islamic legal discourse would revert to more rigid, traditionalist or state-controlled forms, leading to increased internal conflict for progressive Muslims and potentially exacerbating human rights issues in Muslim-majority contexts. The intellectual and social landscape of global Islam would significantly shift.
% FOUNDING_PROBLEM: The perceived irrelevance or ethical conflict of classical Islamic legal rulings with modern societal norms, human rights, and public interest (maslaha), leading to a crisis of faith and legal applicability for many Muslims.
% FOUNDING_PROBLEM_CORROBORATION: Numerous contemporary Islamic scholars, human rights organizations, and progressive Muslim movements attest to the ongoing nature of this problem, citing specific examples of classical rulings that conflict with modern ethical standards. This corroboration comes from outside the traditional authority structures that benefit from the status quo.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while it challenges traditional authority, it still operates within an established religious framework, incurring some costs for those who adopt it (e.g., social friction). Suppression is relatively low (0.30) as this reading actively seeks to lower barriers to alternative interpretations, though it still faces resistance from traditionalists. Theater ratio is low (0.10) as its function is genuinely about re-interpreting and applying law, not performative maintenance. The increasing extractiveness over time reflects the ongoing struggle against traditionalist counter-mobilization, while decreasing suppression indicates a gradual opening of interpretive space.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of progressive Muslims, this is a vital rope for navigating faith in the modern world. From traditional authority structures, it is a threat to established order and a source of fragmentation. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive Muslims, women, LGBTQ+ individuals, and human rights advocates are beneficiaries (low d) as this reading directly addresses their concerns and provides a framework for justice and inclusion. Traditional authority structures are victims/payers (high d) as their interpretive monopoly is challenged, leading to a loss of influence and legitimacy. Secular legal systems are observers, analyzing its impact without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine efforts at coordination and adaptation (rope) as pure extraction (snare) by acknowledging the real benefits it provides to marginalized groups and its function in making Islamic law relevant. Conversely, it avoids mislabeling it as a pure mountain by recognizing the active resistance it faces and the costs it imposes on traditional power structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_adoption_rate,
    'To what extent is this reformist ijtihad reading being adopted and institutionalized within formal Islamic legal bodies, educational institutions, and state legal systems?',
    'Empirical study of curriculum changes in madrasas and universities, fatwa council pronouncements, and judicial rulings in Muslim-majority countries over a 10-20 year period.',
    'Higher institutional adoption would lower the effective suppression and extractiveness for beneficiaries, moving the constraint closer to a pure rope. Low adoption would indicate it remains a contested, niche interpretation, increasing its vulnerability to traditionalist counter-mobilization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_adoption_rate, empirical, 'Measures the real-world impact and stability of the reformist reading.').

omega_variable(
    traditionalist_counter_mobilization,
    'What is the intensity and effectiveness of traditionalist counter-mobilization efforts against this reformist ijtihad reading?',
    'Analysis of traditionalist scholarly publications, social media campaigns, and political lobbying efforts aimed at discrediting or suppressing reformist interpretations.',
    'High counter-mobilization would increase the effective suppression and extractiveness for beneficiaries, potentially pushing the constraint towards a tangled rope or even snare in contexts where traditionalists hold power. Low counter-mobilization would allow the rope-like coordination function to operate more freely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_counter_mobilization, empirical, 'Assesses the external pressures on the reformist reading.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''reformist_ijtihad'' reading of the ''quran_hadith_substrate'' kernel, or is it merely a secularization project using Islamic terminology?',
    'Conceptual analysis of the internal coherence of the interpretive methodology, its grounding in classical Islamic sources (even if re-interpreted), and its acceptance by self-identified Islamic scholars.',
    'If it is deemed a secularization project, its legitimacy within Islamic discourse would be severely undermined, increasing its effective suppression and extractiveness for its proponents. If it is a genuine Islamic reading, its potential for internal reform is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishes genuine internal reform from external imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 10, 0.13).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 20, 0.12).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 30, 0.11).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.1).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 30, 0.33).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_hadith_substrate' kernel. This 'reformist_ijtihad' reading influences the 'traditionalist_taqlid' and 'state_hybrid' readings by challenging their interpretive monopolies and offering an alternative framework for Islamic legal thought.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
