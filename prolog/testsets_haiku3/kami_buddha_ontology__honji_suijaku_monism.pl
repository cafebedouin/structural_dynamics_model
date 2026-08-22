% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   The honji-suijaku monism reading asserts a single ultimate reality in
 *   which kami are phenomenal manifestations (suijaku) of the original ground
 *   (honji) of Buddhist bodhisattvas. This is ONE reading of the contested
 *   kernel of kami-buddha ontology. Under this reading, kami have no
 *   independent ontological standing; they derive their reality entirely from
 *   their bodhisattva substrates. The reading emerged historically around the
 *   8th–12th centuries as Buddhist institutions encountered widespread kami
 *   veneration and sought to establish doctrinal authority and institutional
 *   control. It enabled the incorporation of kami worship into Buddhist
 *   temples and the subordination of local practitioners to Buddhist clerical
 *   hierarchies. This reading claims a genuine metaphysical truth—that there
 *   is a single ground, accessible to Buddhist metaphysics but not to
 *   kami-only practitioners—and uses that claim to justify institutional
 *   extraction from kami practitioners. The constraint exhibits high
 *   suppression because enforcement operates partly through institutional
 *   exclusion (controlling ritual sites, mandating priest training in
 *   Buddhist doctrine) and partly through ontological displacement
 *   (redefining kami as derivative). Theater ratio rises substantially over
 *   time as the framework becomes increasingly systematized and
 *   performance-oriented: the honji-suijaku mappings are elaborated, codified
 *   in treatises, ritualized in ceremonies, and maintained theatrically even
 *   when practitioners' actual devotions diverge from the mapped identities.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy — sets and enforces the honji-suijaku interpretation, controls temple-shrine fusion sites, trains priests, adjudicates kami identities
 *   - Kami worship practitioners — maintain local kami veneration, pay the cost of ontological subordination and institutional displacement, lack direct access to the framework's theoretical justification
 *   - Shinto preservationists — resist the honji-suijaku reading, argue for autonomous kami ontology, constrained by institutional Buddhist dominance
 *   - Syncretist clerical orders — benefit from the framework's systematicity, operate dual-authority sites, collect revenue and ritual authority from both kami and Buddhist sources
 *   - Competing religious readings — excluded from institutional authority by the framework's institutional dominance; would posit kami as autonomous or multiple ultimates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.68).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.72).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.68).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji-Suijaku Monism: Kami as Buddhist Phenomenal Traces").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '3aa1c177-3f1d-45bf-88fa-7f72fd1c959f').
narrative_ontology:cs_kernel_codification('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', fixed_text).
narrative_ontology:cs_authority_grounding('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', lineage).
narrative_ontology:cs_interpretation_layer_present('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f').
narrative_ontology:cs_reading_relation('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', foundational, kami_ontologically_non_autonomous).
narrative_ontology:cs_axiom_status(kami_ontologically_non_autonomous, holdable).
narrative_ontology:cs_axiom_grounding('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', kami_ontologically_non_autonomous, deontological).
narrative_ontology:cs_axiom('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', foundational, single_ultimate_reality_principle).
narrative_ontology:cs_axiom_status(single_ultimate_reality_principle, holdable).
narrative_ontology:cs_axiom_grounding('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', single_ultimate_reality_principle, conventional).
narrative_ontology:cs_reference_frame('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', single_ultimate_ground_with_hierarchical_manifestation).
narrative_ontology:cs_drift_state('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', contemporary_post_meiji_religious_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3aa1c177-3f1d-45bf-88fa-7f72fd1c959f', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, kami_worship_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_preservationists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, syncretist_clerical_order).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, single_ultimate_reality_principle).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, hierarchical_ontological_monism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples, monasteries, and clerical orders promulgate the honji-suijaku framework as the authoritative interpretation of the kami-buddha relationship. They commission theological treatises, train priests in the framework, control temple-shrine fusion sites (jingū-ji), and adjudicate disputes over kami identity by mapping them to specific bodhisattvas. The framework subordinates kami to Buddhism and reserves ultimate ontological authority for Buddhist metaphysics. Institutional survival and doctrinal integrity depend on maintaining this hierarchy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_hierarchy, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Villagers, family groups, and local practitioners maintain kami veneration practices—agricultural fertility rites, protective house shrines, seasonal offerings. Under the honji-suijaku framework, their kami are reinterpreted as manifestations of distant Buddhist entities they may not recognize or worship. Their lived religious practice persists but is theoretically subordinated to a hierarchy they do not control and often do not understand. The framework's suppression operates not through direct prohibition but through ontological displacement: kami are 'real' only as traces of something higher.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kami_worship_practitioners, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, kami_worship_practitioners, excluded).

% Intellectual and ritual specialists (miko, some shrine priests) who argue kami have autonomous ontological standing and are not reducible to Buddhist bodhisattvas. They seek to recover pre-Buddhist or parallel Shinto traditions and resist the subordination entailed by the honji-suijaku reading. Their exit options are constrained by institutional dominance of Buddhist interpretation and by the framework's ability to reabsorb their resistance (they can be reinterpreted as serving a 'higher' kami that is itself a Buddhist manifestation).
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_preservationists, payer,
    moderate, civilizational, constrained, national).

% Buddhist-Shinto fusion priesthoods (especially from the Tendai and Shingon schools) operate dual-site temple-shrine complexes where the honji-suijaku framework is the operational theology. They collect offerings and revenues from both kami veneration (now framed as Buddhist worship) and Buddhist ritual, and their clerical authority rests on their ability to authoritatively map kami to bodhisattvas. They benefit from the framework's systematicity: it legitimates their institutional hybridity and gives them arbitrage—they can serve both constituencies simultaneously under a single hierarchy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, syncretist_clerical_order, beneficiary,
    institutional, civilizational, arbitrage, national).

% Educated lay believers, scholars, and religious patrons who engage with the honji-suijaku framework intellectually and sometimes debate its coherence. They may accept it as true, use it strategically for institutional affiliation, or critique it as forced systematization. Their position is analytical: they witness how the framework operates in lived practice and can testify to tensions between the theory and the practitioners' actual devotions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, lay_theological_community, observer,
    moderate, biographical, mobile, national).

% Non-Buddhist metaphysical frameworks (including post-Meiji Shinto nationalism and pluralist religious philosophies) that would posit kami as autonomous entities, multiple ultimate realities, or a fundamentally different kind of being from Buddhist entities. These readings are structurally barred from institutional authority by the honji-suijaku framework's institutional dominance. Their exclusion is the framework's enforcement cost.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, competing_religious_reading, excluded,
    powerful, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the theological collision between indigenous Japanese kami veneration and institutionalized Buddhism by positing a single ultimate ground: kami are manifestations of bodhisattvas, which allows Buddhist institutions to incorporate and organize kami worship within a unified metaphysical hierarchy without requiring the elimination of kami practice.
% TRANSFER_FUNCTION: Moves doctrinal authority from local kami practitioners to the Buddhist institutional hierarchy; reinterprets kami worship as Buddhist worship performed under Buddhist metaphysical authority; transfers ritual and economic resources (offerings, temple maintenance, priestly fees) into Buddhist-controlled institutional channels; subordinates the ontological status of kami to bodhisattvas, making kami derivative and non-autonomous.
% ABSENT_VOICES: Autonomous-kami readings (kami as independent entities), pre-Buddhist metaphysical frameworks, religious traditions outside the Buddhist-Shinto dyad, and kami practitioners whose lived devotion does not correspond to the mapped bodhisattva identity are structurally excluded from the institutional apparatus that adjudicates the framework's truth. They would object to the subordination and challenge the framework's coherence; their exclusion is the suppression the framework sustains.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku framework disappeared, Japanese religious practice would not. Local kami worship would persist; the question is whether it would be reframed as autonomous religious practice (kami-centered) or absorbed into a different integrating theory (pluralist, non-hierarchical, or post-Buddhist). Buddhist institutions would lose their theoretical apparatus for claiming authority over kami shrines and practitioners. Institutional relationships and real estate would be contested. The world would not 'rearrange' chaotically—there are other stable readings of the kami-buddha relationship—but institutional power would redistribute significantly.
% FOUNDING_PROBLEM: From the 8th century CE onward, Japanese Buddhist institutions encountered an established, widespread, locally rooted kami veneration system. The problem was how to claim religious supremacy and authority over a population whose primary devotions were to kami, and how to incorporate kami into Buddhist doctrine without either eliminating them (which would alienate the population) or treating them as a separate, non-Buddhist reality (which would concede religious authority). The honji-suijaku framework solved this: it made kami theologically continuous with Buddhism and structurally subordinate to Buddhist ontology.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional histories and theological treatises from the 8th–12th centuries (Kūkai, Saichō, and their successors) attest the founding problem and argue for the honji-suijaku solution as the necessary framework for unified religious authority. Contemporary scholars in religious history (e.g., Kuroda Toshio, Sueki Fumihiko) corroborate that the framework arose as an institutional solution to the control problem—but they also note that the 'problem' itself was constructed by Buddhist institutions seeking dominance. Kami practitioners and Shinto preservationists attest the founding problem differently: the problem was Buddhist institutions' demand for supremacy, not an inherent theological conflict. The competing readings offer incompatible genealogies.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, contested).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because the framework systematically transfers doctrinal authority, ritual control, and economic resources from local practitioners to the Buddhist institutional hierarchy. The transfer is disguised as theological truth—kami 'really are' bodhisattva manifestations—but is fundamentally institutional: practitioners lose control over how their kami are identified, what rituals are performed, and who receives the offerings. Suppression is higher still (0.72) because enforcement operates at multiple levels: institutional control of ritual sites and priestly training (structural); ontological displacement that makes autonomous-kami worship theoretically unthinkable within the framework (internalized); and economic pressure (offerings flow through Buddhist temples). Theater ratio (0.58) is moderately high because the honji-suijaku framework is increasingly a performed, elaborated doctrine: complex mappings of individual kami to specific bodhisattvas, theological treatises, ceremonial performances that enact the unity. Yet the framework does solve a real coordination problem—how to hold kami and Buddhist worship together in a single coherent practice. This is why the constraint is claimed as tangled_rope, not pure snare: there is genuine coordination function, but it is so heavily burdened by extraction that the extraction exceeds what coordination requires. The measurement series show rising extractiveness and theater ratio over the interval, consistent with institutional consolidation and increasing systematization of the framework. Suppression requirement rises as institutional enforcement matures and internalization deepens.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institutional seat, the honji-suijaku framework is a truth claim and a unifying doctrine—a coherent metaphysics that brings the entire religious landscape under one ontological canopy. From this seat, the high suppression reflects the cost of maintaining doctrinal unity against forces of religious pluralism and local particularism. From the kami practitioner seat, the same framework is institutional subordination—their kami are reinterpreted as Buddhist, their shrines are absorbed into temple complexes, their authority over their own religious practice is transferred to Buddhist priests who claim to understand the 'true' nature of their kami better than they do themselves. From this seat, the suppression is simple domination dressed in metaphysical language. The engine computes the per-seat type divergence from this structural asymmetry: the institutional seat experiences integration-plus-control (tangled_rope or rope, depending on whether exit is viable); the practitioner seat experiences extraction-plus-displacement (snare or piton, depending on whether the framework persists by institutional force or by internalized acceptance). The two seats compute different types from the same constraint because their structural relationships to it—their exits, their power, their beneficiary/victim status—are fundamentally opposed.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy benefits from the framework (d near 0.0): it gains doctrinal authority, control of ritual and real estate, and integration of a major competitor (kami veneration) into its institutional structure. Its exit options are arbitrage—it can emphasize the Buddhist side or the kami side depending on context, or move between institutions. Kami practitioners are the victims (d near 1.0): they pay the cost of ontological subordination, loss of control over their own religious practice, and integration into an institutional structure they did not create and do not control. Their exit options are identity-locked—they cannot simply abandon kami worship without rupturing core identity; they are embedded in local communities and practice traditions centered on kami. Shinto preservationists are also payers (d elevated): they actively resist the framework but are constrained by its institutional dominance; their exit is constrained—they can preserve alternative readings but only in the margins of institutional structures controlled by the honji-suijaku framework. The lay theological community sits near symmetric (d ≈ 0.5): they may benefit from doctrinal coherence and intellectual engagement, but they also bear the cost of having to navigate a framework they may not fully accept.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is institutionally solved: Buddhist institutions claim doctrinal supremacy over kami, practitioners are incorporated into Buddhist-controlled structures, and a theoretically unified ontology is promulgated. However, there is a significant mandatrophy signal: the founding problem was originally 'how to claim religious authority over a population whose primary devotions are to kami,' and that problem is solved through extraction and suppression, not through genuine kami-practitioner agreement. The framework persists by institutional force and by internalized acceptance—practitioners come to believe the honji-suijaku theory is true, or at least do not question it within the institutional context. If institutional enforcement weakened, the question would be whether the framework would persist because it is genuinely true (in which case the original coordination function remains valid) or whether it would collapse because it was sustained by suppression (in which case mandatrophy has occurred—the original problem no longer generates the constraint's persistence). The high theater ratio (0.58, rising to that level) suggests some performative maintenance: the honji-suijaku relationships are increasingly elaborated and ritualized, but may be doing less actual work of integration and more work of institutional display. A mandatrophy resolution would come from demonstrating that kami practitioners no longer see the framework's truth and only maintain compliance under institutional pressure—that would show the founding problem (institutional dominance) persists, but the ostensible problem (theological incoherence) has been replaced by performative unity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honji_accessibility_constraint,
    'Is the honji (original ground / bodhisattva essence) accessible to practitioners at all, or is it a theoretical posit that kami practitioners have no means to verify or interact with?',
    'Historical record of kami-practitioner attestations: do kami practitioners report direct experience of the bodhisattva identity, or only of the kami manifestation? Do they accept the framework because they experienced its truth, or because institutional authority mandated it?',
    'If honji is inaccessible to practitioners, the framework becomes a pure institutional imposition—extraction without coordination. If accessible (through visualization, ritual, or revelation), it might constitute genuine doctrine. This determines whether the constraint is extractive imposition (snare) or hybrid coordination-plus-extraction (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_accessibility_constraint, empirical, 'Whether the honji-suijaku relationship is experientially available to practitioners or theoretically imposed by hierarchy.').

omega_variable(
    kernel_vs_framework_ambiguity,
    'Is the honji-suijaku relationship a kernel (a fixed, authoritative commitment) or an interpretive framework layered over an ambiguous, unsystematized practice landscape?',
    'Textual analysis of authoritative Buddhist sources: do they assert honji-suijaku as a binding doctrine, or as one possible interpretation among others? Institutional analysis: do Buddhist authorities enforce conformity to the mapping, or tolerate local variation?',
    'If the honji-suijaku is a true kernel (formal, binding, systematized), the constraint is a commitment-system enforcement with clear authority grounding. If it is a framework imposed over an inherently unsystematized domain (kami practice), it is a coercive systematization. This determines the cs_structure.kernel_codification classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_framework_ambiguity, conceptual, 'Whether honji-suijaku is a fixed kernel or an interpretive gloss on plural practices.').

omega_variable(
    suppression_internalization_mechanism,
    'How much of the measured suppression (0.72) is structural (institutional exclusion, economic pressure, control of ritual sites) versus internalized (practitioners accept the framework as true, have fused kami identity with bodhisattva doctrine, find autonomous-kami readings unthinkable)?',
    'Post-framework historical periods (e.g., post-Meiji Shinto nationalism, or contemporary pluralist communities) where the framework''s institutional grip weakens: do kami practitioners immediately revert to autonomous-kami readings, or do they carry the internalized hierarchy forward?',
    'High internalization means the suppression persists even if institutional enforcement weakens; it is psychologically embedded. Low internalization means the constraint depends entirely on institutional maintenance. This affects whether the constraint would dissolve if institutional authority dissolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Structural vs. internalized suppression in the framework''s operation.').

omega_variable(
    extraction_disguised_as_coordination,
    'Does the honji-suijaku framework genuinely solve a coordination problem (how to hold kami and Buddhist worship together coherently), or does it use the appearance of coherence to legitimize Buddhist institutional extraction from kami practitioners?',
    'Comparative analysis: in contexts where kami and Buddhism remained unintegrated (or where other integrating frameworks are available), do practitioners report equivalent coordination problems? Do they perceive the honji-suijaku solution as more coherent than alternatives, or as institutional imposition?',
    'If genuine coordination, the high extractiveness reflects the asymmetric transfer of authority, not coercion. If pure extraction disguised, the coordination function is illusory and the constraint is misclassified (should be snare, not tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_disguised_as_coordination, conceptual, 'Whether honji-suijaku provides real coordination or is a cover story for institutional extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(kami_tr_t0, projected).
narrative_ontology:measurement(kami_tr_t3, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 3, 0.42).
narrative_ontology:measurement_basis(kami_tr_t3, observed).
narrative_ontology:measurement(kami_tr_t6, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 6, 0.48).
narrative_ontology:measurement_basis(kami_tr_t6, observed).
narrative_ontology:measurement(kami_tr_t9, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 9, 0.53).
narrative_ontology:measurement_basis(kami_tr_t9, observed).
narrative_ontology:measurement(kami_tr_t12, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 12, 0.58).
narrative_ontology:measurement_basis(kami_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(kami_be_t0, projected).
narrative_ontology:measurement(kami_be_t3, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(kami_be_t3, observed).
narrative_ontology:measurement(kami_be_t6, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(kami_be_t6, observed).
narrative_ontology:measurement(kami_be_t9, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 9, 0.63).
narrative_ontology:measurement_basis(kami_be_t9, observed).
narrative_ontology:measurement(kami_be_t12, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(kami_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(kami_su_t0, projected).
narrative_ontology:measurement(kami_su_t3, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 3, 0.61).
narrative_ontology:measurement_basis(kami_su_t3, observed).
narrative_ontology:measurement(kami_su_t6, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 6, 0.67).
narrative_ontology:measurement_basis(kami_su_t6, observed).
narrative_ontology:measurement(kami_su_t9, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 9, 0.7).
narrative_ontology:measurement_basis(kami_su_t9, observed).
narrative_ontology:measurement(kami_su_t12, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(kami_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__honji_suijaku_monism, 0.18).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_supremacy_claim).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, temple_shrine_fusion_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings in the kami-buddha-ontology kernel family. The honji-suijaku monism reading asserts single ultimate reality with hierarchical Buddhist grounding; domain_partition asserts functional separation; incoherent_bundle denies coherence. Each reading constructs a different constraint from the same contested kernel. Honji-suijaku monism is the reading most dependent on Buddhist institutional enforcement and generates the highest suppression and theater ratio among the three siblings—it is the most elaborately systematized and the most institutionally costly to maintain. Domain_partition generates lower extraction because it permits operational separation. Incoherent_bundle treats the kernel itself as a cover story, generating highest theater ratio but lowest enforceability (the underlying commitment is denied). All three are linked via network.affects_constraints because the adoption of one reading by institutional actors directly pressures and constrains the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, powerless, 0.85).
constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
