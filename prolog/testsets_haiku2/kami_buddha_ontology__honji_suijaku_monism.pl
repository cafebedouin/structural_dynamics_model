% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji-Suijaku Monism: Kami as Buddhist Phenomenal Traces
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   The honji-suijaku (original-essence/phenomenal-trace) framework
 *   represents one reading of the contested kernel of kami-buddha ontology in
 *   medieval and early-modern Japan. This reading posits a single ultimate
 *   Buddhist reality from which kami emerge as dependent phenomenal
 *   manifestations. Kami are not autonomous entities but traces of
 *   bodhisattvas and Buddhas—subordinate modes of a higher Buddhist truth.
 *   This reading was systematized by Buddhist scholastics and institutional
 *   authorities, and it functions as a coordinating device that allows both
 *   shrine practice and Buddhist doctrine to be unified within a coherent
 *   metaphysical hierarchy. However, it also extracts authority from Shinto
 *   traditions and shrine practitioners by reinterpreting their kami-centered
 *   experience as derivative rather than independent. The constraint is
 *   CLAIMED as rope (coordination through unified framework) while the
 *   metrics describe substantial active enforcement (high suppression) and
 *   extractive authority transfer (high extractiveness rising over the
 *   interval), particularly as institutional power solidified the framework
 *   during the Edo period.
 *
 * KEY AGENTS:
 *   - systematic_buddhist_institutions: Agenda-setters (institutional power, generational horizon) — author and enforce the honji-suijaku framework, extracting interpretive authority
 *   - shrine_practitioners: Payers (powerless, identity-locked to local shrine experience) — bear suppression of their autonomous kami-understanding
 *   - indigenous_shinto_traditions: Payers (moderate institutional power, generational horizon) — lose interpretive autonomy and authority over kami theology
 *   - theoretical_metaphysicians: Beneficiaries (organized, mobile exit) — gain professional careers and prestige from systematizing the framework
 *   - state_administrative_apparatus: Agenda-setter/beneficiary (institutional power) — uses the hierarchical ontology to manage religious institutions and consolidate bureaucratic control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.67).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.58).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.67).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji-Suijaku Monism: Kami as Buddhist Phenomenal Traces").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious/philosophical").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'f4b5c2a6-64e6-4353-a1a8-6f1792dafeab').
narrative_ontology:cs_kernel_codification('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', formalized).
narrative_ontology:cs_authority_grounding('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', lineage).
narrative_ontology:cs_interpretation_layer_present('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab').
narrative_ontology:cs_reading_relation('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', kami_buddha_ontology__domain_partition, influences).
narrative_ontology:cs_reading_relation('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', foundational, single_ultimate_reality_thesis).
narrative_ontology:cs_axiom_status(single_ultimate_reality_thesis, holdable).
narrative_ontology:cs_axiom_grounding('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', single_ultimate_reality_thesis, deontological).
narrative_ontology:cs_axiom('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', foundational, buddhist_ontological_priority).
narrative_ontology:cs_axiom_status(buddhist_ontological_priority, holdable).
narrative_ontology:cs_axiom_grounding('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', buddhist_ontological_priority, deontological).
narrative_ontology:cs_reference_frame('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', buddhist_metaphysical_primacy).
narrative_ontology:cs_drift_state('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', meiji_restoration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f4b5c2a6-64e6-4353-a1a8-6f1792dafeab', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, systematic_buddhist_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, theoretical_metaphysicians).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shrine_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, indigenous_shinto_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, state_administrative_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Temples and monastic orders that adopted and systematized the honji-suijaku reading as part of their institutional authority structure. They claim authority to interpret kami as manifestations of Buddhist bodhisattvas, subordinating Shinto entities within a Buddhist metaphysical hierarchy. This framing legitimated their authority over shrine rituals and allowed them to collect dues and control shrine management during periods of institutional dominance.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, systematic_buddhist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholastic monks, philosophers, and later modern religious scholars who benefited intellectually and professionally from having a coherent systematic framework (honji-suijaku theory) to explain Japanese religious phenomena. The theory solved an apparent contradiction in Japanese religion (simultaneous reverence for kami and buddhas) by subordinating one to the other, creating a prestige-bearing intellectual system for its expositors and making careers available in its exegesis.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, theoretical_metaphysicians, beneficiary,
    organized, biographical, mobile, national).

% Ordinary people at shrines who revered kami as autonomous supernatural beings and sought their favor. Under the honji-suijaku reading, they were told their kami were not independent entities but manifestations of buddhas—a metaphysical claim that subordinated their immediate religious experience within a Buddhist framework. They could not easily exit: refusing the interpretation meant social/legal sanctions from institutional authorities; accepting it meant internalizing the hierarchical ontology.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shrine_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Shinto priestly lineages and family shrine keepers whose authority and interpretive autonomy were systematically subordinated under the honji-suijaku framework. Their kami were reinterpreted as dependent phenomena rather than autonomous powers; their ritual expertise was repositioned as a surface practice hiding Buddhist truths. They were structurally excluded from the theoretical conversation—the framework was authored by Buddhist scholars, not Shinto traditionalists.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, indigenous_shinto_traditions, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, indigenous_shinto_traditions, excluded).

% Alternative readings of the kami-buddha relationship (such as domain partition, which treated kami and buddhas as ontologically distinct but complementary) were suppressed or treated as intellectually primitive by the systematizing institutions. These readings would have preserved Shinto autonomy and interpretive authority; their exclusion from scholarly recognition and institutional legitimacy was enforced through institutional power, not through argument.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, competing_explanatory_frameworks, excluded,
    organized, biographical, trapped, national).

% Government authorities (particularly during the Edo and Meiji periods) that used the honji-suijaku framework to manage religious institutions and consolidate authority. The hierarchical ontology provided a rationale for subordinating Shinto to state-controlled Buddhism, making religious pluralism into a hierarchical system amenable to bureaucratic control. Later, the state could reverse or enforce the interpretation depending on political needs.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, state_administrative_apparatus, beneficiary).

% Modern scholars in religious studies who analyze the honji-suijaku framework as a historical intellectual achievement—a systematic metaphysical theory that resolved apparent contradictions in Japanese religious life, but also as a contestable reading imposed by institutional power rather than empirical necessity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, contemporary_comparative_religionists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, systematic_buddhist_institutions).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the apparent problem of simultaneous reverence for both kami and buddhas in medieval Japanese religion by subordinating kami to Buddhist metaphysics: provides a single coherent ontological framework in which both can be true by treating kami as phenomenal manifestations of underlying Buddhist principles. Enables shrine and temple to operate within a unified religious system rather than as contradictory domains.
% TRANSFER_FUNCTION: Transfers interpretive authority from Shinto traditionalists and shrine practitioners to Buddhist institutions and scholastic philosophers. Shrine practitioners' direct religious experience and autonomy is subordinated to a metaphysical system authored by Buddhist scholars. The arrangement extracts legitimacy and institutional control from shrine systems by reframing them as dependent phenomena rather than autonomous powers.
% ABSENT_VOICES: Shinto traditionalists who would reject the honji-suijaku reading in favor of viewing kami as autonomous beings are structurally excluded from the theoretical conversation. Shrine-level practitioners who experience kami as independent agents are not part of the scholarly consensus that produces this reading. Alternative non-hierarchical readings (such as domain partition, treating kami and buddhas as complementary equals) are suppressed or dismissed as intellectually unsophisticated.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku monism framework disappeared, Japanese religious practice and institutional authority would reorganize: Shinto shrines would reclaim interpretive autonomy over kami; shrine practitioners would no longer be required to understand their kami as Buddhist manifestations; institutional authority would decentralize from Buddhist scholastics to local shrine keepers; the religious pluralism would become explicitly dual-system (Shinto/Buddhism as parallel) rather than hierarchical. The constraint's persistence depends on active enforcement of the metaphysical interpretation by institutional authorities.
% FOUNDING_PROBLEM: Medieval Japanese religion faced an apparent contradiction: people revered kami (indigenous spirits/deities) and buddhas/bodhisattvas simultaneously without clear hierarchy or systematic relationship. The honji-suijaku framework solved this by positing that kami are phenomenal manifestations (suijaku) of an underlying Buddhist reality (honji)—providing metaphysical coherence and allowing a single institutional system to govern both.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional scholars and metaphysicians attest the founding problem was real and the honji-suijaku solution was necessary for systematic coherence. Shinto traditionalists and contemporary scholars attest that the problem was partly institutional (Buddhist temples seeking authority over shrines) rather than purely metaphysical—that Japanese practitioners managed dual reverence quite well without systematic subordination. Modern comparative religionists note that the honji-suijaku framework solved a problem the scholastics created by demanding philosophical systematicity where practitioners were content with pragmatic pluralism.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.67 across the interval (roughly 0-1600 CE, with the strongest institutionalization in Edo), reflecting how the honji-suijaku framework progressively extracts interpretive authority from shrine practitioners and Shinto traditions and consolidates it in Buddhist institutions. Early on (t=0), the framework is a philosophical proposal; by the Edo period (t=25-40), it is enforced through institutional networks, legal restrictions on shrine practice, and state-mandated theological curricula. Suppression requirement rises in parallel (0.38 to 0.58), indicating that maintaining the hierarchical ontology requires active suppression of alternative readings (like domain partition) and suppression of autonomous Shinto interpretive authority. Theater ratio rises more moderately (0.25 to 0.44), suggesting that while the framework has genuine philosophical content and coordinates a real system, an increasing portion of the enforcement activity (in the Edo period) is performative maintenance of the hierarchy rather than solving the original coordination problem.
 *
 * PERSPECTIVAL GAP:
 *   Buddhist institutional seats compute this as genuine rope—a coherent metaphysical system that solved a real problem and continues to coordinate practice. Shrine practitioners and Shinto traditionalists compute it as snare—their experience of kami autonomy is subordinated and suppressed by an external theoretical framework imposed through institutional power. The theoretical metaphysicians compute it as beneficial rope—the framework creates intellectual work and prestige. The state apparatus computes it instrumentally: as long as the framework maintains religious order and hierarchical control, its truth-value is secondary to its administrative utility. The engine computes per-seat classification from power/exit/beneficiary-victim data; this analysis explains why the same structural data produces radically different verdicts depending on which seat's situation you examine.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions and metaphysicians benefit from the framework (d near 0.0-0.2): it legitimates their authority, creates interpretive monopoly, supplies careers. Shrine practitioners and Shinto traditions are targets (d near 0.8-0.9): their autonomy is subordinated, their experience reinterpreted, their alternatives suppressed through legal and institutional mechanisms. The state sits near (d ~0.3-0.4): it benefits from the hierarchical order the framework creates, but it is not the primary beneficiary—it is an agenda-setter that can exploit the arrangement. Competing explanatory frameworks (domain partition) are excluded, not merely suppressed—their suppression is the enforcement object itself (preventing the separation of Buddhist and Shinto authority). The framework's extractiveness depends on active enforcement because shrine practitioners and Shinto lineages would otherwise maintain autonomous interpretive authority; the suppression (0.58 at interval end) reflects the level of force required to prevent reversion to pre-hierarchical pluralism.
 *
 * MANDATROPHY ANALYSIS:
 *   The honji-suijaku framework was built to solve a philosophical problem (the apparent contradiction of simultaneous kami and buddha reverence) and a political problem (unifying religious institutions under a single metaphysical authority). The founding problem is contested: Buddhist scholars attest it was genuine metaphysical incoherence requiring resolution; Shinto traditionalists attest it was artificial—practitioners managed dual reverence without philosophical contradiction, and the 'problem' was created by scholastics demanding systematicity. By the Meiji period (late t=40, projected), the founding problem had partially 'died' in two ways: (1) modern comparative religion offered non-hierarchical analytical frameworks that did not require kami-buddha unity, (2) Meiji state Shinto explicitly rejected the honji-suijaku framework and reinstalled Shinto as autonomous, showing the constraint was not natural necessity but institutional choice. However, the framework persisted in many temple-shrine complexes through inertia and institutional profit-taking, making it a candidate piton by the modern era. The measurements show extractiveness plateau around t=30-40, suggesting the constraint entered a maintenance phase where the original coordination function (solving the contradiction) was less important than the institutional rents it produced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_problem_artificiality,
    'Was the apparent contradiction between kami reverence and buddha reverence a genuine metaphysical problem that required resolution, or an artificial problem created by scholastics demanding philosophical systematicity where practitioners were content with pragmatic pluralism?',
    'Historical and anthropological analysis of pre-Edo Japanese religious practice: if practitioners naturally maintained conceptual separation between kami (local, living, relational) and buddhas (universal, cosmic, salvific) without experiencing contradiction, then the problem was largely institutional. If evidence shows active cognitive dissonance in practitioners, the problem was genuine.',
    'If artificial, the honji-suijaku framework reclassifies from genuine rope (solving a real problem) toward snare (imposing a false unified system to consolidate authority). If genuine, it remains a legitimate coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_problem_artificiality, empirical, 'Whether the kami-buddha coordination problem was metaphysical or institutional in origin.').

omega_variable(
    metaphysical_necessity_vs_institutional_choice,
    'Is the honji-suijaku hierarchy logically necessary given the phenomenology of Japanese religion, or is it one optional reading among several coherent possibilities?',
    'Comparative analysis showing whether domain partition and other non-hierarchical readings are logically possible and can accommodate the same data (dual reverence, institutional coexistence, ritual complement). If multiple incompatible readings equally explain the phenomena, the honji-suijaku monism is institutional choice rather than metaphysical necessity.',
    'If necessary, the framework''s extractiveness is partly justified as the cost of coherence. If optional, the extraction appears as pure institutional power-consolidation disguised as philosophical truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_necessity_vs_institutional_choice, conceptual, 'Whether honji-suijaku monism is the only coherent reading or one option among logically possible alternatives.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.58 at interval end) structural (external legal/institutional barriers to alternative readings) or internalized (shrine practitioners and Shinto scholars came to believe in their own subordination)?',
    'Post-Meiji evidence: when state Shinto explicitly rejected honji-suijaku and reinstated Shinto autonomy, did shrine practitioners and Shinto scholars immediately reclaim independent interpretive authority, or did internalized hierarchical thinking persist? Rapid reclaiming indicates structural suppression; persistent subordination thinking indicates internalization.',
    'If structural, the constraint''s effective suppression drops when enforcement is removed. If internalized, the constraint''s effects persist beyond removal of external force, indicating deeper identity-lock than the structural measure captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative kami readings is external or internalized.').

omega_variable(
    reading_contestation_and_kernel_coherence,
    'Is the kami_buddha_ontology kernel itself coherent, or is the framework''s persistence attributable to institutional power rather than to the coherence of any single reading?',
    'Analysis of whether the incoherent_bundle reading (treating shinbutsu-shugo as a sustained institutional contradiction rather than a coherent system) better explains the historical record than assuming one reading is true and others false. If the constraint literature shows constant reversion to contradiction and requires periodic re-systematization, the kernel may be genuinely incoherent.',
    'If the kernel is incoherent, honji-suijaku monism is not a ''true'' reading of a coherent reality but a performative imposition of false coherence for institutional purposes—reclassifying the constraint from rope toward snare/piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_and_kernel_coherence, conceptual, 'Whether the kami-buddha ontological kernel admits coherent readings or is fundamentally contradictory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(kami_tr_t0, observed).
narrative_ontology:measurement(kami_tr_t5, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(kami_tr_t5, observed).
narrative_ontology:measurement(kami_tr_t10, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(kami_tr_t10, observed).
narrative_ontology:measurement(kami_tr_t15, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(kami_tr_t15, observed).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(kami_tr_t20, observed).
narrative_ontology:measurement(kami_tr_t25, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 25, 0.43).
narrative_ontology:measurement_basis(kami_tr_t25, observed).
narrative_ontology:measurement(kami_tr_t30, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(kami_tr_t30, observed).
narrative_ontology:measurement(kami_tr_t40, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(kami_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(kami_be_t0, observed).
narrative_ontology:measurement(kami_be_t5, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(kami_be_t5, observed).
narrative_ontology:measurement(kami_be_t10, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(kami_be_t10, observed).
narrative_ontology:measurement(kami_be_t15, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(kami_be_t15, observed).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(kami_be_t20, observed).
narrative_ontology:measurement(kami_be_t25, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(kami_be_t25, observed).
narrative_ontology:measurement(kami_be_t30, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(kami_be_t30, observed).
narrative_ontology:measurement(kami_be_t40, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(kami_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(kami_su_t0, observed).
narrative_ontology:measurement(kami_su_t5, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(kami_su_t5, observed).
narrative_ontology:measurement(kami_su_t10, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(kami_su_t10, observed).
narrative_ontology:measurement(kami_su_t15, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(kami_su_t15, observed).
narrative_ontology:measurement(kami_su_t20, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(kami_su_t20, observed).
narrative_ontology:measurement(kami_su_t25, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(kami_su_t25, observed).
narrative_ontology:measurement(kami_su_t30, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(kami_su_t30, observed).
narrative_ontology:measurement(kami_su_t40, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(kami_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__honji_suijaku_monism, 0.12).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, shinbutsu_shugo_institutional_structure).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, meiji_state_shinto_restoration).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel admits three structurally distinct readings, each yielding a different constraint story with different ε values and beneficiary/victim structures. honji_suijaku_monism (this story) posits hierarchical ontological identity with kami as dependent; domain_partition posits ontological distinction with complementary function; incoherent_bundle treats the whole framework as institutionally incoherent rather than coherently singular. The three stories form a constraint family linked by network.affects_constraints. Each reading has its own ε, which is fixed by the reading's own lights (the standing arrangement the reading describes, not the reading's endorsed alternative). The network enables cross-reading analysis of how institutional power determines which reading dominates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
