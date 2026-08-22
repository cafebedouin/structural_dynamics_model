% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Exogenous Override Reading of LDS Marriage Commitment Legitimacy (1890 Manifesto)
 *   domain: religious institutional history / political theology / commitment systems
 *
 * SUMMARY:
 *   This constraint models the 1890 Manifesto under the
 *   exogenous_override_reading: the federal government coerced the LDS Church
 *   into publicly abandoning plural marriage through threats of property
 *   confiscation and disincorporation, while theological doctrine remained
 *   unchanged and practice was merely suspended under duress. The resulting
 *   arrangement creates a persistent legitimacy gap for LDS membership, who
 *   experience a spiritual frame (plural marriage as doctrinal) incompatible
 *   with material conditions (enforced monogamy). The Church leadership
 *   administers the constraint, the federal government captures the
 *   compliance value, and practitioners bear the costs of family dissolution
 *   and doctrinal abandonment. This reading treats the Manifesto as a snare:
 *   the coordination story (prophetic revelation unifying marriage practice)
 *   is cover for pure extraction of institutional compliance.
 *
 * KEY AGENTS:
 *   - federal_government: Primary beneficiary (institutional/arbitrage) â extracts territorial compliance and national norm uniformity
 *   - lds_church_leadership: Agenda setter (institutional/constrained) â administers capitulation under duress, frames it as revelation
 *   - lds_membership: Primary target (organized/identity_locked) â bears legitimacy-gap costs, fused religious identity prevents exit
 *   - plural_marriage_practitioners: Secondary target (powerless/trapped) â families dissolved by direct enforcement, no viable exit
 *   - fundamentalist_dissidents: Excluded voice (moderate/constrained) â reject capitulation, practice covertly, excluded from institutional discourse
 *   - political_theologian_observer: Analytical observer (analytical/analytical) â tracks coercion-to-doctrine translation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.82).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Exogenous Override Reading of LDS Marriage Commitment Legitimacy (1890 Manifesto)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious institutional history / political theology / commitment systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '60ebd0af-bc50-4936-a737-2b1f5b243f92').
narrative_ontology:cs_kernel_codification('60ebd0af-bc50-4936-a737-2b1f5b243f92', fixed_text).
narrative_ontology:cs_authority_grounding('60ebd0af-bc50-4936-a737-2b1f5b243f92', lineage).
narrative_ontology:cs_interpretation_layer_present('60ebd0af-bc50-4936-a737-2b1f5b243f92').
narrative_ontology:cs_reading_relation('60ebd0af-bc50-4936-a737-2b1f5b243f92', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('60ebd0af-bc50-4936-a737-2b1f5b243f92', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('60ebd0af-bc50-4936-a737-2b1f5b243f92', foundational, manifesto_origin_is_federal_coercion).
narrative_ontology:cs_axiom_status(manifesto_origin_is_federal_coercion, holdable).
narrative_ontology:cs_axiom_grounding('60ebd0af-bc50-4936-a737-2b1f5b243f92', manifesto_origin_is_federal_coercion, empirically_contingent).
narrative_ontology:cs_axiom('60ebd0af-bc50-4936-a737-2b1f5b243f92', foundational, doctrine_unchanged_practice_suspended).
narrative_ontology:cs_axiom_status(doctrine_unchanged_practice_suspended, holdable).
narrative_ontology:cs_axiom_grounding('60ebd0af-bc50-4936-a737-2b1f5b243f92', doctrine_unchanged_practice_suspended, empirically_contingent).
narrative_ontology:cs_reference_frame('60ebd0af-bc50-4936-a737-2b1f5b243f92', doctrinal_plural_marriage_framework).
narrative_ontology:cs_drift_state('60ebd0af-bc50-4936-a737-2b1f5b243f92', post_manifesto_1890, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('60ebd0af-bc50-4936-a737-2b1f5b243f92', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, plural_marriage_practitioners).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, federal_supremacy_in_territorial_governance).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, monogamy_as_national_civic_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Extracts institutional compliance from the LDS Church through threats of property confiscation, disincorporation, and denial of statehood, securing territorial integration and uniform national marriage norms.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).

% Administers the Manifesto and enforces monogamous marriage norms among membership under explicit federal duress; publicly frames capitulation as prophetic revelation while privately acknowledging coercive origin; cannot exit federal pressure without institutional destruction.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cognitive and spiritual costs of a legitimacy gap: taught that plural marriage is doctrinal but required to practice monogamy; identity fusion with the Church makes exit costly; many experience the Manifesto as abandonment of a core theological commitment.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    organized, biographical, identity_locked, regional).

% Already-married plural families and practitioners face federal prosecution, disenfranchisement, and social erasure; the constraint directly dissolves their legal and spiritual family structure with no viable exit that preserves their theological commitments.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, plural_marriage_practitioners, payer,
    powerless, immediate, trapped, local).

% Regard the Manifesto as illegitimate capitulation and continue plural marriage covertly or in schismatic communities; excluded from official institutional discourse and subject to excommunication and federal raids.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, fundamentalist_dissidents, excluded,
    moderate, biographical, constrained, regional).

% Analyzes the structural relationship between federal state coercion and religious institutional adaptation; traces how exogenous political pressure reproduces itself as doctrinal suspension and legitimacy crisis.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, political_theologian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement enforces monogamous marriage norms across the LDS community to secure federal political integration and institutional survival, suspending prior plural marriage practice under federal threat of property seizure and disincorporation.
% TRANSFER_FUNCTION: Moves doctrinal legitimacy and marriage practice from plural marriage theology toward federally compliant monogamy, transferring compliance value to the federal state while LDS membership bears spiritual, cognitive, and familial costs.
% ABSENT_VOICES: Polygamous practitioners and theological conservatives who regard the Manifesto as illegitimate capitulation rather than revelation are excluded from institutional governance; they continued practicing plural marriage covertly or formed fundamentalist sects.
% DISAPPEARANCE_RATIONALE: If the coerced suspension vanished overnight, the LDS Church would resume doctrinal plural marriage practice, federal-state relations in the Intermountain West would destabilize, and the legitimacy gap between spiritual framing and material conditions would close or transform into open schism.
% FOUNDING_PROBLEM: Federal-state conflict over polygamy threatening LDS institutional survival, territorial property rights, and Utah statehood admission.
% FOUNDING_PROBLEM_CORROBORATION: Federal officials and anti-polygamy activists attested the coercion from outside; post-Manifesto fundamentalist dissenters attested the problem was 'solved' by capitulation rather than genuine theological resolution; statehood was granted in 1896, corroborating the federal government's framing that compliance ended the crisis.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82) is high because the constraint transfers compliance value to the federal state while imposing persistent spiritual and familial costs on membership. Suppression (0.88) is higher still because persistence depends on federal legal coercion and ecclesiastical discipline suppressing plural marriage alternatives. Theater ratio (0.68) is elevated: the prophetic-revelation framing is performative cover for political capitulation. Accessibility collapse (0.85) is high because once the federal enforcement infrastructure and ecclesiastical discipline are in place, alternatives (open plural marriage) become virtually inaccessible. Resistance (0.52) is moderate: covert practice and schismatic movements persist but are fragmented and heavily suppressed. Temporal trajectories show gradual normalization as federal pressure recedes post-statehood, but extraction persists because the legitimacy gap becomes structurally embedded.
 *
 * PERSPECTIVAL GAP:
 *   The federal government seat computes the constraint as low-extraction coordination (it secured national norms and territorial peace at low ongoing cost). The LDS membership and practitioner seats compute it as high-extraction snare (doctrine suspended, families destroyed, identity leveraged to prevent exit). The Church leadership seat sits between: it experiences coercion as target but enacts the constraint as agenda setter, producing a split computed directionality that the engine resolves from structural data rather than authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government is the structural beneficiary (collects compliance, secures statehood integration â d near beneficiary pole). LDS membership and plural marriage practitioners are the structural targets (bear doctrinal abandonment costs, trapped or identity-locked â d near full target). LDS Church leadership is the agenda setter administering the constraint under external coercion; its derived directionality sits ambiguously between beneficiary (institutional survival) and target (federal duress), but the structural data weights it toward agenda-setter administration rather than rent capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfederal-state conflict over polygamy threatening institutional survivalâwas resolved by statehood admission in 1896, yet the constraint persisted and normalized. Under the exogenous_override reading, this persistence is not piton (inertial theater without beneficiaries) because the federal government continues to capture the value of uniform national marriage norms and the Church continues to enforce the suspended practice to maintain its institutional position. The classification as snare prevents misreading the persistence as mere inertia: there is a concentrated beneficiary (federal government) and identifiable victims (membership, practitioners), so the mandate has not atrophied into piton but rather hardened into a structure of extracted compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_causation_ambiguity,
    'Did the Manifesto originate primarily from federal coercion, or from internal prophetic deliberation responding to political conditions?',
    'Archival discovery of federal communication timelines versus internal Church deliberation records; comparative analysis of the Woodruff diary and federal enforcement directives.',
    'If endogenous deliberation is proven, the constraint''s classification shifts from snare toward tangled_rope or rope; if pure exogenous coercion is confirmed, snare classification hardens and the legitimacy gap reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_causation_ambiguity, empirical, 'Ambiguity about whether Manifesto origin is federal coercion or prophetic response').

omega_variable(
    doctrine_practice_gap_persistence,
    'Does the doctrinal commitment to plural marriage persist unaltered beneath the monogamous practice, or was theological reinterpretation silently accomplished?',
    'Analysis of correlated theological texts, quorum teachings, and member belief surveys across the 1890â1920 interval; detection of doctrinal shift in official versus unofficial discourse.',
    'If doctrine was silently reinterpreted, the exogenous_override reading''s core axiom is falsified and the constraint''s victim structure changes (members may be coordinated rather than extracted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap_persistence, empirical, 'Whether plural marriage doctrine persisted unchanged under monogamous practice').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (federal legal coercion and ecclesiastical discipline) or internalized (members policing themselves through theological guilt and identity fusion)?',
    'Compare federal prosecution and church court rates against post-1920 persistence of monogamy after federal pressure receded; trajectory of internalized guilt versus external enforcement.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after external coercion is removed, strengthening the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_contest,
    'How would the structural classification change if the endogenous_reinterpretation_reading or hybrid_pragmatic_reading were adopted instead of the exogenous_override_reading?',
    'Cross-reading comparison of the same historical evidence set; evaluation of which reading better predicts institutional behavior (e.g., post-Manifesto fundamentalist persistence, leadership private statements).',
    'An endogenous reading removes the federal beneficiary and recasts membership as coordinated rather than extracted; a hybrid reading distributes agency between divine and political causation, likely producing tangled_rope rather than snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural delta across sibling readings of the marriage commitment legitimacy kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.75).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 5, 0.7).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 10, 0.65).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 5, 0.83).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The natural-language label '1890 Manifesto' conflates three structurally distinct claims about marriage commitment legitimacy. This file isolates the exogenous_override_reading (federal coercion, high extraction, identifiable victims). Sibling files isolate the endogenous_reinterpretation_reading (genuine revelation, coordination function) and hybrid_pragmatic_reading (strategic adaptation, mixed agency). They are linked as a constraint family because they share the same historical kernel but instantiate different epsilon values, beneficiary structures, and classification types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
