% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Kami-Buddha Ontology as Incoherent Institutional Bundle
 *   domain: religious_studies/philosophy_of_religion
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-buddha synthesis) in medieval and early-modern
 *   Japan consolidated heterogeneous religious commitments — Shinto kami
 *   veneration and Buddhist practice — into a single institutional and ritual
 *   framework without resolving their ontological incompatibility. The
 *   constraint that emerges is not a kernel (no stable, authoritative text or
 *   principle) but an institutionally sustained bundle of contradictory
 *   commitments. Shrine-temples function dual roles through performative
 *   incoherence: rituals contradict each other doctrinalally but succeed
 *   pragmatically. Theoretical attempts to resolve the contradiction
 *   (honji-suijaku monism, domain partition) remain scholarly, not
 *   institutional. The system persists because institutional inertia
 *   (shrine-temples cannot afford doctrinal purity) and priestly
 *   identity-lock (priests define themselves as synthesists) sustain the
 *   incoherence. This reading instantiates the 'incoherent bundle' kernel
 *   reading, distinct from sibling readings that posit coherent ontology
 *   (honji-suijaku monism) or strict functional separation (domain
 *   partition).
 *
 * KEY AGENTS:
 *   - syncretic_shrine_temples: Institutional agenda-setter — maintains dual ritual function through incoherence
 *   - priestly_institutional_class: Beneficiary and secondary agenda-setter — professional identity locked into synthesis role
 *   - doctrinal_purists: Payer — exhausted by need to explain away syncretism without institutional power to change it
 *   - reform_movements: Payer and excluded — attempted to enforce single ontology; institutional cost of exit exhausted without changing parent system
 *   - lay_ritual_practitioners: Beneficiary — experience unified sacred space without consciousness of contradiction
 *   - doctrinal_theorists: Observer — analyze the contradiction but lack institutional authority to resolve it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.62).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, piton).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Kami-Buddha Ontology as Incoherent Institutional Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/philosophy_of_religion").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '02167b2a-1c3e-4129-a326-23c7c8cd05d1').
narrative_ontology:cs_kernel_codification('02167b2a-1c3e-4129-a326-23c7c8cd05d1', distributed).
narrative_ontology:cs_authority_grounding('02167b2a-1c3e-4129-a326-23c7c8cd05d1', extraction).
narrative_ontology:cs_interpretation_layer_present('02167b2a-1c3e-4129-a326-23c7c8cd05d1').
narrative_ontology:cs_reading_relation('02167b2a-1c3e-4129-a326-23c7c8cd05d1', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('02167b2a-1c3e-4129-a326-23c7c8cd05d1', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('02167b2a-1c3e-4129-a326-23c7c8cd05d1', foundational, institutional_incoherence_irreducible).
narrative_ontology:cs_axiom_status(institutional_incoherence_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('02167b2a-1c3e-4129-a326-23c7c8cd05d1', institutional_incoherence_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('02167b2a-1c3e-4129-a326-23c7c8cd05d1', foundational, ritual_efficacy_independent_of_doctrinal_coherence).
narrative_ontology:cs_axiom_status(ritual_efficacy_independent_of_doctrinal_coherence, holdable).
narrative_ontology:cs_axiom_grounding('02167b2a-1c3e-4129-a326-23c7c8cd05d1', ritual_efficacy_independent_of_doctrinal_coherence, empirically_contingent).
narrative_ontology:cs_reference_frame('02167b2a-1c3e-4129-a326-23c7c8cd05d1', institutional_pragmatism_without_doctrinal_mandate).
narrative_ontology:cs_drift_state('02167b2a-1c3e-4129-a326-23c7c8cd05d1', contemporary_religious_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('02167b2a-1c3e-4129-a326-23c7c8cd05d1', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, syncretic_shrine_temples).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, priestly_institutional_class).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, doctrinal_purists).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, reform_movements).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as PITON: extractive (0.58), sustained by institutional inertia and theatrical maintenance (theater_ratio 0.71), with no party benefiting enough to maintain it and no party hurt enough to fix it. The extractiveness comes from the priestly class's ability to extract authority and income by sustaining incoherence — they collect from lay practitioners through ritual fees and cultural legitimacy while refusing the cost of doctrinal consistency. Suppression (0.62) is sustained through institutional refusal to recognize the contradiction as a legitimate problem — doctrinal purists are treated as sectarian extremists, and reform attempts are marginalized as modernizing impositions. Theater is extremely high (0.71) because the entire system operates through performative success despite theoretical failure: rituals 'work' regardless of their incompatibility, and priests celebrate this pragmatic efficacy as vindication rather than acknowledging it as mask for incoherence. Accessibility collapse (0.41 at individual level, 0.72 at organizational level) is asymmetric: institutional actors (shrine-temples, priests) find alternatives nearly impossible (they would lose legitimacy and income); individual lay practitioners maintain mobility — they can attend other institutions or abandon practice. Resistance (0.55 at structural level initially, declining to 0.42 by interval end) captures the historical trajectory: Meiji reformers, Buddhist modernizers, and Shinto nativists mounted significant resistance (late 1800s–early 1900s) but institutional inertia outlasted their energy. The coercion grid shows suppression rising faster at organizational level (0.62 to 0.72) than at individual level (0.08 to 0.12), indicating the institutional machinery hardened against challenge while lay experience remained uncoerced. Resistance decays across all levels as the system normalized and reformers exhausted themselves.
 *
 * PERSPECTIVAL GAP:
 *   Institutional agents (shrine-temples, priests) experience this as ROPE — genuine coordination providing unified sacred infrastructure that neither kami-only nor buddha-only systems could supply. From their seat, the 'incoherence' is a feature, not a bug: it allows simultaneous service to both functions without choosing. Doctrinal purists and reformers experience it as SNARE — extraction of authority by institutional refusal to acknowledge the contradiction, with suppression of their attempts to enforce coherence. Lay practitioners experience it as near-ROPE or MOUNTAIN — unified infrastructure, low friction, ritual efficacy they do not question. The engine computes per-seat classifications from this structural asymmetry: institutional actors will compute nearer beneficiary/rope, doctrinal seats nearer victim/snare, lay seats nearer beneficiary/rope or mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic shrine-temples sit near full beneficiary (d ≈ 0.15–0.25): they collect extraction from lay practitioners (fees, donations, cultural legitimacy) without running an alternative system. They have constrained exit but face institutional loss, not personal deprivation, if the system changed. Priestly institutional class sits at full beneficiary (d ≈ 0.1): their entire professional identity and income flows from the synthesis role; exit options are identity-locked — leaving the priesthood is leaving themselves. Their directionality is beneficiary but with a twist: they do not collect from the system as an external agent; they ARE the system. Doctrinal purists sit near full target (d ≈ 0.8–0.9): they bear the cost of explaining away or abandoning syncretic practice within their communities; they have constrained exit (leaving means sectarian separation); they derive no benefit from the arrangement. Reform movements sit at moderate target (d ≈ 0.6–0.7): they bore high institutional cost to attempt separation (building rival institutions, losing membership), but they retained mobile exit (they could leave entirely, which purists cannot without abandoning their religion). Lay practitioners sit near symmetric (d ≈ 0.45–0.55): genuine coordination benefit, indirect cost through fee structure, mobile exit (can attend other institutions or abandon practice). Doctrinal theorists sit at observer (d ≈ 0.5, analytical): they derive neither extraction nor subsidy from the constraint; they observe it as data.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a degraded PITON, not a false MOUNTAIN. The founding problem (how to serve both kami and buddhas) was solved by institutional practice, not doctrine. Mandatrophy is RESOLVED: the institutional mandate to explain the kami-buddha relationship has become obsolete — institutions no longer attempt coherent explanation; they perform efficacy. The mandate persists theatrically — priests articulate high-minded rhetoric about 'practical harmony' and 'complementary functions' — but the real function is now ONLY institutional survival and extraction. If the mandate were still live, the system would be under pressure to resolve the contradiction or admit incoherence; instead, the system has simply declared the contradiction a non-problem ('this is how our tradition works') and moved on. The theater ratio (0.71) captures this mandatrophy: the ritual performance is the entire remaining function; the doctrinal mandate has atrophied. The constraint persists not because anyone maintains the mandate but because no one has the institutional power to dismantle the syncretic system — it is cheaper to let shrine-temples coast on historical legitimacy than to force doctrinal purity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_inertia_vs_structural_necessity,
    'Is the shinbutsu-shugo bundle sustained by institutional inertia (path dependency, switching costs) or by genuine structural necessity (the unified sacred infrastructure truly cannot be replicated by separate institutions)?',
    'Comparative analysis of contemporary religious systems in Japan and other East Asian contexts that successfully separated kami/buddha functions without major loss of ritual efficacy or lay participation.',
    'If inertia: the constraint is a pure piton — remove the institutional actors and the bundle dissolves. If necessity: parts of the constraint are genuine coordination with legitimacy as rope or mountain. This determines whether the constraint''s persistence is explained by power/extraction or by actual unsolvability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_structural_necessity, empirical, 'Whether institutional persistence reflects inertia or structural problem-solving.').

omega_variable(
    theoretical_incoherence_vs_hidden_coherence,
    'Is shinbutsu-shugo genuinely incoherent at the ontological level, or does a coherent kernel exist but remain untheorized or institutionally suppressed (e.g., a consistent non-dualistic ontology that would accommodate both kami and buddhas if explicitly formulated)?',
    'Systematic analysis of theological and philosophical sources across Tendai, Shingon, and Shinto schools to test whether a stable, latent ontology underlies the incoherent practices — or rigorous demonstration that no coherent interpretation would satisfy all institutional actors simultaneously.',
    'If hidden coherence exists: the incoherent-bundle reading is a political/institutional framing masking a coherent kernel (the distribution of readings shifts). If genuine incoherence: this reading stands and the other readings are cover narratives. This resolves whether the kernel itself is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theoretical_incoherence_vs_hidden_coherence, conceptual, 'Whether the perceived incoherence masks a hidden coherent ontology or is genuinely irreducible.').

omega_variable(
    priestly_identity_lock_mechanism,
    'What component of priestly identity-lock is professional (economic dependency on the synthesis role) versus ideological (self-concept constituted through the synthesis role) versus relational (self-concept dependent on recognition by both kami and buddha communities)?',
    'Ethnographic documentation of how contemporary priests articulate their professional identity when facing pressure to choose coherent doctrine; analysis of which type of identity-lock assertion dominates.',
    'If primarily economic: the constraint is sustained by incentives (could be reformed by restructuring compensation). If primarily ideological or relational: the constraint is sustained by meaning-making (would require deep cultural shift to reform). This determines repair strategies and whether the constraint is accessible to policy intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_identity_lock_mechanism, empirical, 'Composition of identity-lock sustaining priestly institutional participation.').

omega_variable(
    ritual_efficacy_independence,
    'Does the measured ritual efficacy (healing, blessing, rite-of-passage success, ancestor appeasement) empirically depend on the metaphysical coherence of the kami-buddha relationship, or does efficacy persist independently of theoretical coherence?',
    'Comparative measurement of ritual outcomes (participant satisfaction, perceived healing/blessing success, life outcomes post-ritual) between coherent-doctrine adherents and incoherent-bundle adherents, controlled for confounds (expectation, placebo, self-selection).',
    'If efficacy requires coherence: incoherence is structurally unstable (should collapse under its own dysfunction, predicting mandatrophy). If efficacy is independent: the incoherence is stable (institutional form matters more than doctrinal content — the constraint is a self-sustaining piton). This determines the terminal state of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_efficacy_independence, empirical, 'Whether ritual efficacy depends on doctrinal coherence or operates independently.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the incoherent-bundle reading logically foreclose the other two readings (honji-suijaku monism and domain partition), or do all three readings coexist as live institutional and scholarly positions?',
    'Documentary analysis of institutional practice: can a single shrine-temple simultaneously instantiate incoherent-bundle operation (in ritual practice) and honji-suijaku interpretation (in doctrinal teaching), or are these mutually exclusive frames?',
    'If foreclosure: the other readings are cover stories; the bundle reading is the only honest description. If coexistence: all three readings are live, and the kernel is genuinely contested. This determines the reading_relations field in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between the incoherent-bundle reading and its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.45).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__incoherent_bundle, theater_ratio, 100, 0.52).
narrative_ontology:measurement(kami_tr_t200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 200, 0.58).
narrative_ontology:measurement(kami_tr_t400, kami_buddha_ontology__incoherent_bundle, theater_ratio, 400, 0.68).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__incoherent_bundle, theater_ratio, 600, 0.71).
narrative_ontology:measurement(kami_tr_t800, kami_buddha_ontology__incoherent_bundle, theater_ratio, 800, 0.71).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(kami_be_t200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(kami_be_t400, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(kami_be_t800, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 800, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(kami_su_t100, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 100, 0.44).
narrative_ontology:measurement(kami_su_t200, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 200, 0.5).
narrative_ontology:measurement(kami_su_t400, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 400, 0.59).
narrative_ontology:measurement(kami_su_t600, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 600, 0.62).
narrative_ontology:measurement(kami_su_t800, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 800, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__incoherent_bundle, 0.12).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel admits three structurally distinct constraint readings: honji_suijaku_monism (coherent monistic ontology, low ε, rope-type), domain_partition (coherent dualistic ontology, moderate ε, tangled-rope type), and incoherent_bundle (no coherent kernel, higher ε, piton type). Each reading has a distinct boundary-drawing logic and institutional consequences. The three stories form a constraint family; all three must be generated to model the contested kernel accurately. This reading (incoherent_bundle) argues that the apparent coherence-seeking in the other two readings is itself a theoretical cover for institutional incoherence — the sibling readings are scholarly attempts to render coherent what the institution deliberately keeps incoherent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, institutional, 0.18).
constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
