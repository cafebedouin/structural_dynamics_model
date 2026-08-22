% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Orthographic Reform via Literacy Maximization (Instrumentalist Reading)
 *   domain: political/linguistic/commitment_systems
 *
 * SUMMARY:
 *   An orthographic reform replaces classical (Arabic) script with a
 *   modernized, simplified writing system, justified entirely through
 *   measurable literacy gains and administrative efficiency. The
 *   instrumentalist reading treats the script choice as a pragmatic tool: the
 *   goal is to maximize the fraction of the population that can read and
 *   write and to streamline governmental record-keeping. The constraint's
 *   legitimacy rests on literacy statistics, processing times, and
 *   cost-per-literate-citizen metrics—not on narratives of civilizational
 *   identity or continuity with tradition. The newly literate population
 *   benefits from rapid skill acquisition and access to bureaucratic
 *   participation. The Arabic-literate elite loses the value of their
 *   acquired expertise and the institutional premium that once attached to
 *   their literacy. Religious authorities and classical scholars are
 *   structurally excluded from the instrumentalist frame because their
 *   objections (tradition-preservation, sacred-text access) have no standing
 *   in a discourse organized around efficiency metrics. This reading coexists
 *   with the continuity_reading (which emphasizes tradition and religious
 *   text access) and the modernist_reading (which adds identity-political
 *   claims about rupture from the Ottoman past and alignment with Europe).
 *   The three readings share the orthographic enforcement machinery but
 *   instantiate different constraint structures because they name different
 *   victims and legitimacy sources.
 *
 * KEY AGENTS:
 *   - administrative_apparatus: institutional agenda-setter, controls script policy and enforcement
 *   - newly_literate_population: powerless beneficiary, gains mass access to literacy and bureaucratic participation
 *   - arabic_literate_elite: powerful victim, loses skill value and institutional authority
 *   - religious_authorities: excluded from instrumental frame, would object on tradition grounds
 *   - modernizing/colonial power: observer, supplies technical legitimacy and international support
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.58).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.52).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Orthographic Reform via Literacy Maximization (Instrumentalist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political/linguistic/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, 'b2130442-6a88-4633-9353-8776c1d53f42').
narrative_ontology:cs_kernel_codification('b2130442-6a88-4633-9353-8776c1d53f42', formalized).
narrative_ontology:cs_authority_grounding('b2130442-6a88-4633-9353-8776c1d53f42', extraction).
narrative_ontology:cs_interpretation_layer_present('b2130442-6a88-4633-9353-8776c1d53f42').
narrative_ontology:cs_reading_relation('b2130442-6a88-4633-9353-8776c1d53f42', orthographic_legitimacy_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2130442-6a88-4633-9353-8776c1d53f42', orthographic_legitimacy_kernel__modernist_reading, influences).
narrative_ontology:cs_axiom('b2130442-6a88-4633-9353-8776c1d53f42', foundational, script_choice_is_pragmatic_tool).
narrative_ontology:cs_axiom_status(script_choice_is_pragmatic_tool, holdable).
narrative_ontology:cs_axiom_grounding('b2130442-6a88-4633-9353-8776c1d53f42', script_choice_is_pragmatic_tool, instrumental).
narrative_ontology:cs_axiom('b2130442-6a88-4633-9353-8776c1d53f42', foundational, literacy_maximization_justifies_reform).
narrative_ontology:cs_axiom_status(literacy_maximization_justifies_reform, holdable).
narrative_ontology:cs_axiom_grounding('b2130442-6a88-4633-9353-8776c1d53f42', literacy_maximization_justifies_reform, empirically_contingent).
narrative_ontology:cs_reference_frame('b2130442-6a88-4633-9353-8776c1d53f42', classical_script_elite_literacy).
narrative_ontology:cs_drift_state('b2130442-6a88-4633-9353-8776c1d53f42', post_reform_mass_literacy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b2130442-6a88-4633-9353-8776c1d53f42', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, administrative_apparatus).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, colonial_or_modernizing_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the orthographic standard as the official script for government documents, education, and public administration. Justifies the script choice entirely through measurable literacy gains and efficiency metrics: speed of acquisition, administrative processing time, cost per literate citizen. Controls curriculum design, examination standards, and official communications.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains access to written language and administrative participation through the new script. Literacy rates rise measurably and quickly under the new system (empirically faster acquisition than traditional script learning paths). They lack the resources to demand a different script and benefit directly from the coordination function—a single, standardized writing system for national communication and record-keeping.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    powerless, biographical, constrained, national).

% Experiences devaluation of acquired literacy skills and cultural authority. Their decades of education in classical Arabic script become administratively obsolete; the bureaucracy no longer rewards their expertise. They must either relearn the new script (at cost) or accept reduced administrative influence. They retain social status and wealth but lose the direct connection between literacy and state power that previously justified their elite position.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, payer,
    powerful, biographical, constrained, national).

% Are structurally excluded from the reform's legitimacy discourse. The instrumentalist reading frames the question as purely technical—literacy rates and administrative efficiency—leaving no opening for objections grounded in religious tradition or sacred-text preservation. Their resistance ('this script desecrates the Quran') is treated as obstruction of rational progress rather than as substantive claims deserving a seat at the deliberation.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, religious_authorities, excluded,
    powerful, generational, trapped, national).

% Often supplies the technical expertise, curriculum models, and international legitimacy for the reform (if colonial). May benefit indirectly from reduced friction in administrative communication and reduced costs of operating within the bureaucracy. Sits in an analytical frame but has structural interest in the script's success—their models, funding, and political alignment are stake-holders in the reform's continuation.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, colonial_or_modernizing_power, observer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, colonial_or_modernizing_power, beneficiary).

% Document and analyze the transition; retain access to historical texts in the old script but observe the breaking of the living chain of transmission. They become custodians of a dead language as the new script becomes the native literacy for the next generation. Their work becomes archaeological rather than participatory.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, historical_scholars, observer,
    moderate, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, administrative_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, standardized orthographic system that reduces the costs of literacy acquisition and allows rapid administrative scaling. Instead of requiring mastery of a complex classical script with decade-long learning curves, the new system enables basic literacy in 1–2 years, permitting mass incorporation into bureaucratic participation, record-keeping, and commercial communication.
% TRANSFER_FUNCTION: Transfers cultural authority and the tangible salary/status premium attached to literacy from the classical-script-educated elite to the newly literate population and the state apparatus that now certifies a different form of literacy. The devaluation of Arabic literacy is the cost imposed on one party to achieve coordination gains for the majority.
% ABSENT_VOICES: Religious authorities and classical-text scholars who would object on grounds of tradition-preservation and civilizational continuity are structurally excluded from the instrumental frame—the discourse is organized around measurable literacy rates and efficiency, leaving no standing for tradition-based objections. Indigenous language advocates (if the reform privileges a colonial script) are similarly excluded.
% DISAPPEARANCE_RATIONALE: If the orthographic mandate vanished, literate bureaucracies would revert to mixed-script systems or return to the classical script; literacy rates would plateau at pre-reform levels; administrative processing would slow; the newly literate population would lose institutional incentive to maintain the new skill; the cultural authority of the classical-script elite would be restored. The coordinated administrative apparatus depends on the script mandate's persistence.
% FOUNDING_PROBLEM: Pre-reform literacy rates were low (often <10% of population); classical script training required years of study and was accessible only to elites; administrative processing was slow and errors frequent due to scribal bottlenecks; the state lacked the capacity to incorporate common people into bureaucratic roles because reading and writing were not mass skills.
% FOUNDING_PROBLEM_CORROBORATION: The administrative apparatus and modernizing reformers attest the founding problem was acute and the reform solved it—literacy statistics and processing-time data support this reading. Classical-script elites and religious authorities contest the framing, arguing that the founding problem was not literacy but elite capture of writing, and that the reform solved administrative efficiency at the cost of breaking the textual tradition. Historians from outside the benefiting parties document rising literacy rates but also document the break in institutional transmission of classical texts and the narrowing of the knowledge base that remained accessible to mass literacy.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end), rising from 0.35 at the start. Early in the reform, the constraint functions nearly as pure rope: genuine coordination benefit (mass literacy), modest extraction (the elite's skill devaluation is real but offset by broad gains). Over the interval, extractiveness rises as the reform matures—the administrative apparatus progressively tightens enforcement of the new script, defunds classical-script education, and reorganizes bureaucratic incentives around the new literacy standard. By year 30, the rising trajectory plateaus: extractiveness stabilizes because the generational replacement is complete; the new script becomes native literacy for the majority, and the old script becomes a heritage artifact. Suppression follows a similar trajectory, rising from 0.38 to 0.52 as the apparatus actively suppresses classical-script institutions and excludes continuity-based objections from policy discourse. Theater (performative vs. functional activity) starts low (0.08)—the reform's early justifications rest on measurable literacy gains—but rises to 0.31 by year 30 as the original coordination problem fades and the apparatus increasingly defends script standardization through ceremonial iteration of literacy statistics rather than genuine empirical improvements. The shared time grid ensures all three metrics are authored at every measured point; the plateau after year 30 reflects stabilization of the constraint once generational lock-in is complete.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and the newly literate population should compute this constraint as rope or mild scaffold (coordination function dominant); the elite should compute it as snare (their exit options are trapped—they cannot unlearn the new script's dominance, and the old script's value is systematically suppressed). The engine computes per-seat classifications from the power/exit/beneficiary/victim data; this story authors structural data that predict the gap. The gap is the point: it is not an error but the evidence that the constraint distributes its benefits and costs asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   The administrative apparatus is the structural beneficiary and agenda-setter: it controls the script policy and collects the efficiency gains (reduced processing time, faster turnaround, lower training costs). Its directionality is low (beneficiary end, d ≈ 0.15–0.25). The newly literate population benefits from coordination—rapid literacy access, participation in administration—but also bears the cost of abandoning the classical script tradition. For them, directionality is near-symmetric (d ≈ 0.45–0.55): real benefit, distributed cost. The Arabic-literate elite are the primary victims: their accumulated expertise becomes administratively worthless, they must relearn or accept declining influence, they pay through skill devaluation. Their directionality is high (d ≈ 0.75–0.85). Religious authorities are excluded from the discourse entirely—the instrumental framing has no seat for tradition-based objections, so their directionality cannot be computed from beneficiary/victim data (they are neither). The engine's directionality derivation runs on beneficiary/victim declarations; the exclusion is structural and drives an omega variable, not a directionality override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—low literacy rates and slow administrative processing—was live at the reform's outset. By year 30, literacy is near-universal and administrative efficiency is normalized; the founding problem is functionally dead. Yet the constraint persists and even strengthens its suppression (year 30 suppression = 0.52, unchanged from year 25, indicating enforcement has stabilized at a higher level than early-reform needs required). This is a classic mandatrophy signal: a constraint built to solve a problem that has been solved, now maintained through institutional inertia and theater. The theater_ratio rising from 0.08 to 0.31 is the diagnostic: by year 30, the reform is justified through ceremonial citation of literacy statistics (which no longer vary meaningfully) rather than through the actual coordination work (which was completed by year 15). The constraint's persistence beyond the founding problem's life is evidence that something other than coordination efficiency now sustains it—likely the administrative apparatus's preference for standardized control and the irreversibility of the lock-in for the next generation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_rate_causation,
    'Do literacy rates rise because the new script is genuinely faster to acquire, or because the new script enjoys massive institutional support and the old script is defunded and delegitimized, making the comparison unfair?',
    'Controlled comparison of acquisition curves when both scripts receive equal institutional support; or analysis of literacy rates in regions where both scripts remain parallel options.',
    'If the new script''s advantage is intrinsic, the coordination function is real and the extraction modest. If the advantage comes entirely from institutional leverage, the measured extractiveness understates the constraint''s true coercive character—the beneficiary has engineered a comparison that favors the new script by suppressing the alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_rate_causation, empirical, 'Whether script-choice advantage is intrinsic or artifact of institutional support.').

omega_variable(
    instrumentalist_vs_identity_framing,
    'Is the orthographic choice truly pragmatic and script-neutral (a tool choice), or is it itself an identity assertion—a collective affirmation of modernity, rupture from Ottoman/Islamic past, or alignment with European scripts—masquerading as instrumental?',
    'Analysis of the reform''s legitimacy discourse: does it rest purely on literacy statistics and efficiency metrics, or does it include identity-laden framings (modernity, progress, civilization, rupture from tradition)? Does the chosen script correlate with Western European precedent in ways that go beyond ergonomic fit?',
    'If the choice is purely instrumental, the constraint is correctly classified as rope with moderate extraction. If identity assertion is bundled into the instrumental frame but not acknowledged, the constraint should be reclassified upward—it is a modernist_reading (constraint family sibling) with higher extraction and stronger suppression of continuity advocates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instrumentalist_vs_identity_framing, conceptual, 'Whether instrumentalist framing masks identity-assertion reading.').

omega_variable(
    reversibility_and_lock_in,
    'Could the orthographic choice be reversed once a generation of children has been educated in the new script, or does it create irreversible lock-in?',
    'Historical cases where orthographic choices were reversed or remained contested over generations (Ottoman-to-Latin script transitions, post-colonial Latin-to-indigenous-script movements); analysis of institutional coupling between script choice and administrative infrastructure.',
    'If reversible (high exit options for a future generation), the suppression measure is overstated and the constraint is closer to pure rope. If lock-in is irreversible (identity_locked or trapped exit for the next generation), suppression is understated and the constraint approaches tangled_rope or snare, depending on how the next generation experiences the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_and_lock_in, empirical, 'Path-dependency and institutional lock-in of orthographic choice.').

omega_variable(
    reading_contest_vs_constraint_identity,
    'Is this constraint fundamentally about the CHOICE of script (what distinguishes the three sibling readings), or is it about the ENFORCEMENT of script standardization—a coordination problem that would exist in any reading?',
    'Compare the three readings'' structural data: if all three readings share the same enforcement mechanisms, beneficiary/victim structure, and directionality but differ only in their legitimacy narratives, the constraint is really about enforcement and the reading difference is rhetorical. If the readings differ in who benefits and who pays, they are genuinely three different constraints.',
    'If the readings are purely narrative variants of the same structural constraint, decompose the constraint family differently—author one enforcement-structure story and three reading_relations omegas rather than three separate constraint stories. If the readings have genuinely different beneficiary/victim structures (as expected here), keep three separate stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_vs_constraint_identity, conceptual, 'Whether reading difference implies structural constraint difference or narrative overlay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(orth_tr_t5, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orth_be_t5, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(orth_su_t5, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__instrumentalist_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% The orthographic_legitimacy_kernel constraint family decomposes into three structurally distinct constraints, each instantiating a different reading of the kernel. This story (instrumentalist_reading) treats script choice as a pragmatic coordination problem around literacy and efficiency. The continuity_reading emphasizes preservation of historical and religious text access; the modernist_reading adds identity-political claims about Westernization and rupture. The three readings share the enforcement machinery but instantiate different beneficiary/victim structures, different legitimacy discourses, and different ε values. Each reading is a separate constraint story because the ε-invariance principle requires: if changing the observable used to evaluate the constraint (from 'literacy impact' to 'identity assertion' to 'tradition access') changes ε, the observer is looking at different constraints. The three readings produce three different constraints, linked via network.affects_constraints (affects_constraints edges run from each reading to the other two, bidirectional influence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
