% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Qur'an 9:5 Contextual Defensive Reading: Treaty-Violation Response Framework
 *   domain: religious/political theology
 *
 * SUMMARY:
 *   The contextual-defensive reading of Qur'an 9:5 interprets the verse as
 *   addressing a specific 7th-century Medinan context: treaty violations by
 *   polytheist tribes who had breached covenants with the Muslim polity.
 *   Under this reading, 9:5 does not abrogate prior peaceful verses (8:61,
 *   60:8-9); rather, it establishes the permissibility of defensive warfare
 *   specifically against documented treaty violators. The reading prioritizes
 *   treaty obligations, defensive necessity, and the binding nature of
 *   covenants. It benefits integrationist Muslim-majority states seeking to
 *   institutionalize legal frameworks that permit religious pluralism and
 *   minority coexistence. This reading competes with two sibling
 *   interpretations: the abrogating-universal reading (which treats 9:5 as
 *   negating all peace verses and establishing perpetual offensive jihad) and
 *   the progressive-synthesis reading (which historicizes 9:5 as time-bound
 *   political directive, not eternally binding law). The contextual-defensive
 *   reading occupies a middle position: accepting 9:5 as still-binding legal
 *   command but narrowing its scope to the contexts originally
 *   addressed—treaty violation and defensive necessity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.18).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.12).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.18).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Qur'an 9:5 Contextual Defensive Reading: Treaty-Violation Response Framework").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/political theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6').
narrative_ontology:cs_kernel_codification('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', fixed_text).
narrative_ontology:cs_authority_grounding('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', lineage).
narrative_ontology:cs_interpretation_layer_present('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6').
narrative_ontology:cs_reading_relation('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', quran_9_5_scope__abrogating_universal, coexists_with).
narrative_ontology:cs_reading_relation('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', quran_9_5_scope__progressive_synthesis, influences).
narrative_ontology:cs_axiom('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', foundational, historical_context_narrows_scope).
narrative_ontology:cs_axiom_status(historical_context_narrows_scope, holdable).
narrative_ontology:cs_axiom_grounding('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', historical_context_narrows_scope, deontological).
narrative_ontology:cs_axiom('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', foundational, covenant_fidelity_primacy).
narrative_ontology:cs_axiom_status(covenant_fidelity_primacy, holdable).
narrative_ontology:cs_axiom_grounding('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', covenant_fidelity_primacy, deontological).
narrative_ontology:cs_reference_frame('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', medina_treaty_obligation_framework).
narrative_ontology:cs_drift_state('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', contemporary_pluralist_institutionalization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3ba1e7d7-4808-48bb-b7cc-01147a9ddfa6', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, peaceful_pluralist_jurisprudence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, religious_minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt the contextual-defensive reading to ground legal frameworks permitting religious pluralism, minority coexistence, and treaty-based peace. The reading legitimates state authority to enforce treaties with non-Muslim polities and restricts armed response to documented treaty violations or prior aggression. Benefits institutional stability when the reading is adopted as canonical interpretation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, arbitrage, global).

% Academic, clerical, and lay communities promoting coexistence-centered Islamic jurisprudence. The contextual-defensive reading vindicates their interpretive tradition by establishing textual authority for limiting warfare to defensive/treaty-responsive contexts. Does not extract rents; benefits from canonical legitimacy of their hermeneutical method.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, peaceful_pluralist_jurisprudence, beneficiary,
    organized, generational, mobile, global).

% Represents historical Sunni juridical consensus that treated 9:5 within bounded contexts (treaty violation, defensive necessity) rather than as universal abrogation. Observer position: the constraint codifies their historical authority structure's outcome, not their direct current authority.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, historical_maturidi_ashari_schools, observer,
    institutional, civilizational, analytical, global).

% Communities and scholars who hold the competing abrogating-universal reading, which interprets 9:5 as establishing perpetual offensive jihad until universal conversion or submission. Are not in the conversation when the contextual-defensive reading is adopted; would object that limiting 9:5 to treaty violators contradicts the verse's apparent scope and theological mandate.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, abrogating_universal_reading_adherents, excluded,
    organized, generational, constrained, global).

% Scholars advancing the progressive-synthesis reading, which accepts the historical context but treats 9:5 as time-bound political directive superseded by Qur'anic ethical trajectory. Are excluded from the contextual-defensive framing, which treats 9:5 as still-binding legal command (albeit narrowed to treaty violations).
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, progressive_synthesis_scholars, excluded,
    organized, generational, constrained, global).

% Non-Muslim minorities in Muslim-majority states. Benefit indirectly when the contextual-defensive reading is institutionalized: it grounds legal frameworks that protect covenant relationships and restrict armed violence to responses against prior treaty violation or aggression. Their safety depends on state adoption of bounded rather than universal readings.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, religious_minority_communities, beneficiary,
    powerless, biographical, trapped, national).

% Islamic scholars, jurists, and religious authorities who author and defend interpretations of 9:5. The contextual-defensive reading is advanced by historical and contemporary exegetes (mufassirūn) citing historical context, parallel verses on treaty obligation, and the principle that specific contexts limit general commands. They argue for this reading's textual grounding and its coherence with broader Qur'anic ethics.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, quranic_hermeneutics_interpreters, agenda_setter,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, diffuse).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for Muslim-majority polities to maintain treaty relationships with non-Muslim populations and states: the reading coordinates defensive necessity (justified only against prior violation or aggression) with obligations of covenant fidelity. Solves the interpretive problem of reconciling 9:5 with peace-promoting verses (8:61, 60:8-9) by reading 9:5 as a specific response to documented treaty breach.
% TRANSFER_FUNCTION: Transfers interpretive authority from literalist universal readings to historically-grounded contextual ones: the reading shifts which hermeneutical framework counts as canonical, which affects which legal regimes become legitimate (pluralist vs. conquest-oriented). No extraction of material goods; the transfer is normative authority over how the text is read.
% ABSENT_VOICES: Abrogating-universal scholars and progressive-synthesis scholars are absent from decision-making when this reading is institutionalized. They would argue: abrogating-universal adherents contend the verse scope is not narrowed to treaty violators; progressive scholars contend that historical context establishes the directive is time-bound, not eternally binding. Neither voice is included when the contextual-defensive reading is adopted as state jurisprudence.
% DISAPPEARANCE_RATIONALE: If this reading disappeared—i.e., if integrationist Muslim-majority states abandoned the contextual-defensive interpretation and shifted toward abrogating-universal or progressive-synthesis framings—the world would rearrange in contested ways. States might shift from treaty-bounded frameworks to conquest-oriented postures (abrogating reading) or secular-relativist ones (progressive reading). Which would occur depends on which alternative reading is adopted; the disappearance of this reading would force institutional reorganization, but the outcome is disputed.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced the exegetical problem of reconciling Q 9:5 with earlier revealed verses commanding peace, respecting covenants, and protecting non-combatants. The contextual-defensive reading resolved this by historicizing 9:5 to the specific Medinan context of treaty violations by polytheist tribes, preserving peace verses as governing where no prior violation occurred.
% FOUNDING_PROBLEM_CORROBORATION: Sunni jurisprudential consensus (Māturīdī, Ashʿarī schools) historically treated 9:5 as context-bounded. Contemporary Islamic scholars outside the benefiting polities (e.g., academic specialists in Islamic intellectual history, non-aligned scholars) attest that historical evidence supports the contextual reading: documented Medinan treaty violations by Quraysh polytheists, textual parallelism with 8:56 (verse on covenant breach), and consistent Qur'anic commands to honor covenants. However, abrogating-universal scholars dispute this corroboration as incomplete, citing the verse's linguistic scope and its position within Sūrat al-Barāʾah.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, contested).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the reading limits the constraint to defensive/treaty-responsive contexts rather than universal offensive framing; no party is systematically victimized—the constraint protects rather than extracts. Suppression is minimal (0.12) because the reading's operation does not require active suppression of competing interpretations to maintain itself; it stands on textual and historical grounding. Theater is negligible (0.08) because the constraint's function is genuine: resolving exegetical tension between verses, grounding legal frameworks for coexistence. Accessibility collapse is moderate (0.42) because alternatives (the abrogating and progressive readings) remain live and contestable; no reading has achieved total foreclosure of alternatives. Resistance is high (0.71) because the competing abrogating-universal and progressive-synthesis readings mount active intellectual resistance; scholarly debate is vigorous and unresolved. The measurement series tracks the reading's institutionalization: low extraction initially (defensive framing), modest rise as it becomes majority scholarly orthodoxy, slight increase during colonial challenges, stabilization at contemporary levels as pluralist states adopt it institutionally.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (hermeneutics interpreters) and the beneficiary seats (integrationist states, pluralist jurisprudence) should compute identically or nearly so: both benefit from the reading's canonical authority. The excluded seats (abrogating and progressive scholars) would compute this as snare or tangled_rope if they had voice: they experience the contextual-defensive reading as imposing constraints on their interpretive freedom without benefiting them. The engine's per-seat computation should reveal this seat-divergence: the reading appears as rope from inside the adopting institutional framework, as a suppressive constraint from the excluded competing readings' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are integrationist Muslim-majority states (d near 0.2—they collect institutional legitimacy for pluralist frameworks but do not extract material rents from the reading's operation) and pluralist jurisprudence communities (d near 0.15—they benefit from canonical hermeneutical authority without direct material extraction). There are NO victims in this reading: the constraint does not systematically extract from any identifiable group. Religious minorities benefit incidentally (d near 0.3 for trapped minorities whose safety depends on state adoption of this reading). Abrogating-universal and progressive scholars are excluded rather than targeted. This asymmetry (beneficiaries without victims) is structural: the reading's benefit is normative authority and institutional legitimacy, not material extraction. The engine should compute this as rope-type coordination across nearly all seats because the constraint solves a genuine exegetical problem without victimizing anyone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling 9:5 with peace verses and covenantal commands) remains contested—abrogating-universal scholars deny it was a real problem, treating 9:5 as straightforward abrogation; progressive scholars treat it as solved by historicization rather than contextual reading. The disappearance verdict is contested for the same reason: different readings would rearrange institutional frameworks differently. There is no mandatrophy here (the reading's founding problem has not atrophied); the reading remains functionally tied to the exegetical work it was designed to do. The measurement series shows stable suppression because the constraint is not maintained theatrically—it stands on genuine hermeneutical grounds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_scope_ambiguity,
    'Does Q 9:5''s linguistic scope—''slay them wherever you find them''—permit contextual narrowing to treaty violators, or does the apparent generality of the command override contextual interpretation?',
    'Hermeneutical analysis of Qur''anic principles for interpreting general/specific language; comparison with other verses employing similar general commands; scholarly consensus on the validity of contextual restriction in Islamic jurisprudence.',
    'If general language can be narrowed by context, the contextual-defensive reading holds; if generality overrides context, the abrogating-universal reading gains structural support. This is fundamentally a question about hermeneutical method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_scope_ambiguity, conceptual, 'Whether general Qur''anic language can be contextually restricted or whether generality establishes universal scope.').

omega_variable(
    historical_verification_of_treaty_violations,
    'What documentary evidence establishes that specific polytheist tribes violated covenants with the Medina polity in the period addressed by Q 9:5?',
    'Historical analysis of early Islamic sources (Sīra, Maghāzī texts) for explicit treaty-violation claims; cross-checking with non-Muslim historical sources; scholars outside the benefiting communities assessing documentary strength.',
    'If treaty violations are well-documented, the contextual reading''s historical grounding is strengthened; if documentation is sparse or disputed, the reading''s contextual anchor becomes weaker and abrogating readings gain ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_verification_of_treaty_violations, empirical, 'Historical evidence for the treaty-violation context the reading relies on.').

omega_variable(
    abrogation_vs_specification,
    'Is 9:5 best understood as negating prior peace verses (abrogation/nasikh), or as specifying the conditions under which prior commands apply (contextual specification)?',
    'Jurisprudential analysis of abrogation theory in Islamic legal methodology; examination of whether abrogation principle requires complete negation or permits conditional specification; scholarly consensus on the distinctions.',
    'If abrogation requires complete negation, 9:5 abrogates peace verses universally (supporting abrogating reading); if contextual specification is jurisprudentially valid, the contextual-defensive reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_vs_specification, conceptual, 'Whether 9:5 operates as abrogation (negating prior verses) or specification (narrowing their scope).').

omega_variable(
    covenantal_ethics_primacy,
    'In Qur''anic ethical hierarchy, do covenantal obligations and the command to honor treaties (8:56, 16:91-94) take precedence over or subordinate to military directives like 9:5?',
    'Systematic review of Qur''anic covenantal ethics; analysis of which ethical principles courts treat as foundational; scholarship on the integration of military and peace verses in Islamic jurisprudence.',
    'If covenantal ethics are foundational, the contextual-defensive reading (which subordinates warfare to covenant fidelity) is structurally supported; if military directives are foundational, abrogating readings gain support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenantal_ethics_primacy, conceptual, 'The hierarchical relationship between Qur''anic covenantal ethics and military directives.').

omega_variable(
    reading_adoption_incentives,
    'To what extent does institutional adoption of the contextual-defensive reading serve the material interests of integrationist Muslim-majority states seeking pluralist legal frameworks, versus reflecting genuine hermeneutical conviction?',
    'Historical analysis of when and why states adopted this reading; comparison with states that rejected it; assessment of whether adoption correlates with pluralism goals or follows from independent exegetical reasoning; testimony from scholars across different state contexts.',
    'If adoption is primarily interest-driven, the reading''s extraction level is higher (institutional capture of hermeneutics); if adoption reflects genuine scholarly judgment across diverse contexts, extraction remains low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_adoption_incentives, preference, 'Whether institutional adoption of this reading reflects hermeneutical judgment or institutional interests in pluralism.').

omega_variable(
    excluded_voices_legitimacy,
    'Do the excluded abrogating-universal and progressive-synthesis readings represent coherent jurisprudential positions grounded in Qur''anic evidence, or are they methodologically unsound?',
    'Scholarly assessment of the hermeneutical grounding for each competing reading; analysis of which readings have support from historically established schools of thought; evaluation of methodological rigor.',
    'If excluded readings are methodologically sound, the contextual-defensive reading''s institutional dominance requires some suppression; if excluded readings are weaker jurisprudentially, the contextual reading''s prevalence reflects genuine scholarly judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_voices_legitimacy, conceptual, 'The jurisprudential legitimacy and methodological soundness of competing readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qura_tr_t200, quran_9_5_scope__contextual_defensive, theater_ratio, 200, 0.06).
narrative_ontology:measurement(qura_tr_t600, quran_9_5_scope__contextual_defensive, theater_ratio, 600, 0.07).
narrative_ontology:measurement(qura_tr_t1000, quran_9_5_scope__contextual_defensive, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(qura_tr_t1200, quran_9_5_scope__contextual_defensive, theater_ratio, 1200, 0.09).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__contextual_defensive, theater_ratio, 1400, 0.08).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(qura_be_t200, quran_9_5_scope__contextual_defensive, base_extractiveness, 200, 0.12).
narrative_ontology:measurement(qura_be_t600, quran_9_5_scope__contextual_defensive, base_extractiveness, 600, 0.14).
narrative_ontology:measurement(qura_be_t1000, quran_9_5_scope__contextual_defensive, base_extractiveness, 1000, 0.16).
narrative_ontology:measurement(qura_be_t1200, quran_9_5_scope__contextual_defensive, base_extractiveness, 1200, 0.18).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__contextual_defensive, base_extractiveness, 1400, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(qura_su_t200, quran_9_5_scope__contextual_defensive, suppression_requirement, 200, 0.09).
narrative_ontology:measurement(qura_su_t600, quran_9_5_scope__contextual_defensive, suppression_requirement, 600, 0.1).
narrative_ontology:measurement(qura_su_t1000, quran_9_5_scope__contextual_defensive, suppression_requirement, 1000, 0.11).
narrative_ontology:measurement(qura_su_t1200, quran_9_5_scope__contextual_defensive, suppression_requirement, 1200, 0.12).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__contextual_defensive, suppression_requirement, 1400, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__contextual_defensive, 0.06).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The constraint family qur'an_9_5_scope decomposes a single contested Qur'anic verse into three structurally distinct constraints, each with different ε values and victim sets. The contextual_defensive reading (this story) treats 9:5 as context-bounded to treaty violations with low extraction (ε=0.18) and no victims. The abrogating_universal reading (sibling constraint) treats 9:5 as universally abrogating peace verses with substantially higher extraction (ε~0.65+) and victims including all non-Muslim populations. The progressive_synthesis reading treats 9:5 as time-bound historical directive superseded by Qur'anic ethical trajectory, with minimal extraction (ε~0.10) as it is no longer binding law. These are not alternative measurements of one constraint; they are three different constraints unified by their shared textual referent (the verse) but differing in scope, victim set, and structural function. The constraint family is linked via this network edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
