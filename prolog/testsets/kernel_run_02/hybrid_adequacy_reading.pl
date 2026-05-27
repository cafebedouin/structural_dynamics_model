% ============================================================================
% CONSTRAINT STORY: hybrid_adequacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_adequacy_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_adequacy_reading
 *   human_readable: Hybrid Adequacy Reading of Latin Correctness
 *   domain: historical_linguistics/philology/renaissance_intellectual_history
 *
 * SUMMARY:
 *   The hybrid adequacy reading of Latin correctness emerges in 15th–16th
 *   century Renaissance scholarship as a pragmatic response to the conflict
 *   between classical authority and contemporary utility. Working jurists,
 *   physicians, and natural philosophers need Latin vocabulary for concepts
 *   (diseases, legal instruments, astronomical phenomena) unknown to the
 *   classical authors whose texts ground the authority structure. The strict
 *   Ciceronian reading insists on purity — all correct Latin derives from
 *   Cicero's usage; innovation is corruption. The hybrid adequacy reading
 *   permits neologism and technical vocabulary innovation while maintaining
 *   classical stylistic and structural norms as the baseline. This creates a
 *   tangled_rope constraint: the reading benefits working scholars
 *   (functional adequacy for technical discourse) while constraining them
 *   (must justify neologisms through classical authority, maintain façade of
 *   classical decorum). The constraint operates at multiple institutional
 *   levels: academies that control which neologisms gain legitimacy, printers
 *   who create distribution channels for texts, and individual scholars whose
 *   careers depend on acceptability within the orthodox framework. The
 *   theater_ratio reflects that much formal Latin correctness discourse
 *   continues performatively (citing classical authorities, defending
 *   stylistic choices) even as actual practice has become flexible — many
 *   Renaissance texts use neologisms and technical vocabulary that Cicero
 *   never employed, while maintaining surface deference to classical norms.
 *
 * KEY AGENTS:
 *   - Working Scholars (Jurists, Physicians, Scientists): Moderate power (moderate/constrained) — primary beneficiaries of functional adequacy but constrained by legitimacy requirements; must frame innovations in classical language
 *   - Strict Ciceronians: Powerless within the hybrid reading (powerless/trapped) — victims of the boundary shift; their purity standard is displaced; trapped by their commitment to an increasingly marginalized orthodoxy
 *   - Orthodox Grammatical Authority: Institutional (institutional/constrained) — maintains formal authority through inertia (cited lineage, pedagogical prestige) while actual practice drifts beyond their prescriptions; constrained because they cannot fully suppress neologism without losing institutional relevance
 *   - Renaissance Humanist Academies: Institutional (institutional/arbitrage) — primary institutional beneficiaries with power to choose which reading wins endorsement; can switch between strict recovery and hybrid adequacy based on practical needs and patronage networks
 *   - Vernacular Printing Movement: Organized agents (organized/mobile) — see Latin adequacy as transitional; mobile because print vernaculars offer an exit path as technical authority gradually shifts from Latin to national languages
 *   - Analytical Observer: Universal scope (analytical/analytical) — risks treating the hybrid adequacy reading as a linguistic necessity when it is actually an institutional choice backed by Renaissance patronage networks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_adequacy_reading, 0.38).
domain_priors:suppression_score(hybrid_adequacy_reading, 0.48).
domain_priors:theater_ratio(hybrid_adequacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_adequacy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hybrid_adequacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hybrid_adequacy_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_adequacy_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_adequacy_reading, "Hybrid Adequacy Reading of Latin Correctness").
narrative_ontology:topic_domain(hybrid_adequacy_reading, "historical_linguistics/philology/renaissance_intellectual_history").

domain_priors:requires_active_enforcement(hybrid_adequacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_adequacy_reading, 'b0bd08b9-8614-4c2d-84a5-85d705cc0774').
narrative_ontology:cs_created_at('b0bd08b9-8614-4c2d-84a5-85d705cc0774', '').
narrative_ontology:cs_kernel_codification('b0bd08b9-8614-4c2d-84a5-85d705cc0774', fixed_text).
narrative_ontology:cs_authority_grounding('b0bd08b9-8614-4c2d-84a5-85d705cc0774', lineage).
narrative_ontology:cs_interpretation_layer_present('b0bd08b9-8614-4c2d-84a5-85d705cc0774').
narrative_ontology:cs_kernel_id(hybrid_adequacy_reading, latin_correctness).
narrative_ontology:cs_reading_relation('b0bd08b9-8614-4c2d-84a5-85d705cc0774', textual_recovery_reading, forecloses).
narrative_ontology:cs_reading_relation('b0bd08b9-8614-4c2d-84a5-85d705cc0774', living_drift_reading, influences).
narrative_ontology:cs_axiom('b0bd08b9-8614-4c2d-84a5-85d705cc0774', foundational, classical_baseline_non_negotiable).
narrative_ontology:cs_axiom_status(classical_baseline_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('b0bd08b9-8614-4c2d-84a5-85d705cc0774', classical_baseline_non_negotiable, conventional).
narrative_ontology:cs_axiom('b0bd08b9-8614-4c2d-84a5-85d705cc0774', foundational, functional_adequacy_supplements_fidelity).
narrative_ontology:cs_axiom_status(functional_adequacy_supplements_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('b0bd08b9-8614-4c2d-84a5-85d705cc0774', functional_adequacy_supplements_fidelity, instrumental).
narrative_ontology:cs_reference_frame('b0bd08b9-8614-4c2d-84a5-85d705cc0774', classical_ciceronian_purity).
narrative_ontology:cs_drift_state('b0bd08b9-8614-4c2d-84a5-85d705cc0774', renaissance_technical_expansion, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_adequacy_reading, working_scholars_law_medicine_science).
narrative_ontology:constraint_victim(hybrid_adequacy_reading, strict_ciceronians).
narrative_ontology:constraint_victim(hybrid_adequacy_reading, orthodox_grammatical_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRICT CICERONIAN PURIST (SNARE) — Trapped by the normative authority of classical models. Cannot exit the constraint without abandoning their entire textual canon and legitimacy claim. Experiences the hybrid reading as violation of what 'Latin correctness' means. Zero degrees of freedom — purity is their constraint.
constraint_indexing:constraint_classification(hybrid_adequacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WORKING JURIST / PHYSICIAN (TANGLED ROPE) — Benefits from vocabulary innovation enabling technical terminology (neologisms for diseases, legal concepts unknown to Cicero) while constrained by professional legitimacy requirements (must cite classical authorities, maintain stylistic decorum). Extraction runs both directions: gains functional adequacy, loses stylistic purity. Has real agency through tactical innovation but cannot openly reject classical models.
constraint_indexing:constraint_classification(hybrid_adequacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RENAISSANCE HUMANIST ACADEMY (ROPE) — Institutional beneficiary with arbitrage options. Can switch between strict recovery norms (Textual Recovery reading) and hybrid adequacy based on practical need. Controls which reading gets institutional endorsement. Experiences the constraint as a coordination mechanism: managing the tension between classical authority and contemporary utility enables both scholarship and social prestige.
constraint_indexing:constraint_classification(hybrid_adequacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ORTHODOX GRAMMATICAL AUTHORITY (PITON) — Institutional actor that maintains formal authority over correctness standards while the actual practice has drifted beyond them. The authority persists through inertia (cited lineage, pedagogical habit, institutional prestige) even though working scholars increasingly ignore the purity requirement. Theater ratio high because formal pronouncements about 'correct Latin' continue even as the boundary has become plastic.
constraint_indexing:constraint_classification(hybrid_adequacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: VERNACULAR PRINTING MOVEMENT (SCAFFOLD) — Organized agents (printers, poets, merchants) see Latin adequacy as a temporary coordination problem with a sunset clause. As print vernaculars mature and Latin loses its monopoly on technical authority, the constraint weakens. Hybrid adequacy is a transitional reading — necessary while Latin still carries prestige but destined to fade as technical Latin specializes. Low experienced extraction because the coalition sees an exit path (shift to vernacular for merchant/technical texts).
constraint_indexing:constraint_classification(hybrid_adequacy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LINGUISTIC NECESSITY (MOUNTAIN) — From a civilizational perspective, all living languages face the inherent tension between tradition (inherited forms) and innovation (new referents). This perspective treats the hybrid adequacy constraint as an immutable property of language evolution — not a contingent institutional arrangement but a structural feature of how linguistic communities manage meaning over generations. However, the structural data contradicts the mountain gate: the constraint's extractiveness and suppression reflect real asymmetries of power (who controls correctness definitions) that are not universal linguistic law.
constraint_indexing:constraint_classification(hybrid_adequacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_adequacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_adequacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_adequacy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_adequacy_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_adequacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from strict Ciceronians (who lose their monopoly on correctness) and from working scholars (who gain functional vocabulary but must constantly justify innovations through appeals to classical authority, bearing a performance burden). The extraction is genuine but not total — the hybrid reading offers real functionality gains compared to strict Ciceronism (estimated 0.55–0.65 extractiveness). The measurement trajectory shows rising extractiveness (0.32 → 0.45) reflecting accumulating neologisms and looser classical adherence over the 100-year interval, indicating that the boundary between 'acceptable innovation' and 'corruption' gradually shifts, requiring constant renegotiation. Suppression (0.48): Moderate-high. Multiple barriers constrain working scholars: (a) must maintain stylistic coherence with classical models despite using non-classical vocabulary; (b) innovations require institutional approval or tacit acceptance (cannot openly reject classical authority); (c) career risk of being labeled as corrupting Latin if innovations are too bold; (d) strict Ciceronians can still block publication or advancement through gatekeeping. However, suppression is lower than strict Ciceronism (~0.65) because the reading creates explicit space for innovation, even if constrained. Theater ratio (0.55): Moderate-high. A significant portion of Latin correctness discourse is performative — scholars cite classical authorities to justify neologisms, construct classicizing syntax around technical vocabulary, and frame innovations as necessary supplements to classical Latin rather than departures from it. The performance increases over time (0.48 → 0.62) as the gap between classical purity and actual practice widens, requiring more elaborate justification rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates acute perspectival divergence. The strict Ciceronian sees the hybrid reading as a corruption (Snare from their perspective — trapped by their commitment to purity, watching standards collapse). The working scholar sees genuine improvement but also constraint (Tangled Rope — gains functionality but loses stylistic freedom and must constantly justify choices). The Renaissance academy sees coordination (Rope — managing the relationship between classical authority and practical utility). The orthodox grammatical authority sees its own degradation (Piton — formal authority persisting through inertia while actual practice ignores prescriptions). The vernacular movement sees a temporary coordination problem with a sunset (Scaffold — Latin adequacy is necessary now, but as print vernaculars mature, Latin loses monopoly on technical writing). The civilizational analytical observer risks treating the constraint as a linguistic necessity (Mountain — all language communities must balance innovation and tradition) when it is actually an institutional choice backed by specific Renaissance power networks. The engine's false summit detector would flag the mountain classification, revealing the naturalization of a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent position and exit options. Strict Ciceronians (powerless/trapped) occupy d ≈ 0.95 — they experience the constraint at maximum severity because they cannot exit without abandoning their entire intellectual identity and textual canon. Working scholars (moderate/constrained) occupy d ≈ 0.60 — constrained by legitimacy requirements and career risk, but with some exit options (can gradually introduce neologisms, can move to contexts with looser standards, can frame innovations conservatively). Renaissance academies (institutional/arbitrage) occupy d ≈ 0.10 — they are beneficiaries with exit options; they can choose which reading to endorse based on pragmatic needs. The orthodox grammatical authority (institutional/constrained) occupies d ≈ 0.55 — they benefit from maintaining the constraint's existence (prestige from authority) but are constrained by the fact that actual practice drifts beyond their prescriptions, forcing constant renegotiation of authority boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how a single kernel (latin_correctness) can support multiple readings with genuinely different extractiveness profiles. The hybrid adequacy reading reduces extraction compared to strict Ciceronism by approximately 0.15–0.20 (from ~0.55–0.65 to ~0.38–0.40), making it a real structural improvement for working scholars. However, it does not eliminate extraction — it redistributes and masks it through the performance of classical deference. The constraint is tangled_rope, not rope, because the reading requires active enforcement (academies must approve neologisms, scholars must maintain façade of classical authority) and benefits some agents (innovative scholars, institutional actors who control the boundary) while harming others (strict Ciceronians, scholars whose innovations fall outside the approved boundary). The mandatrophy is resolved by recognizing that all three readings (hybrid adequacy, strict Ciceronian, textual recovery, living drift) are live institutional positions with different extractiveness profiles — the choice between them is not a scientific question about what Latin 'really is' but a political question about which constraint framework wins institutional backing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    correctness_definition_kernel,
    'Is ''Latin correctness'' a recoverable historical fact (what Cicero and the classical corpus actually did), a prescriptive norm (what should be done to maintain Latin as a unified language), or a functional pragmatics (what enables communication in the contemporary context)?',
    'Historical analysis of classical usage vs. attestation; comparison of strict purity readings against post-Ciceronian Latin texts (ecclesiastical, medical, legal); longitudinal tracking of which definitions winning institutional authority actually changes scholarly practice',
    'If historical fact: textual recovery reading dominates; hybrid adequacy is deviation. If prescriptive norm: hybrid adequacy reflects a different prescriptive choice (functional over aesthetic). If pragmatics: hybrid adequacy is the obvious structural solution and strict Ciceronism is ideological cover for orthodoxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correctness_definition_kernel, conceptual, 'What ''Latin correctness'' fundamentally means across readings').

omega_variable(
    neologism_boundary_plasticity,
    'Does the hybrid adequacy reading actually enable working scholars to innovate freely, or does it construct a new boundary (classical core + sanctioned technical neologisms) that is equally constraining, just less visible?',
    'Textual analysis of medieval and Renaissance medical/legal Latin; count of neologisms that gain institutional approval vs. those rejected; interview data on how working scholars negotiate the classical-adequacy tension in practice; correlation between stated permission and actual publication pressure',
    'If genuinely enabling: hybrid adequacy reduces suppression from ~0.65 (pure Ciceronism) to ~0.35 (open innovation). If newly constraining: suppression remains high but is masked by the appearance of flexibility. Changes whether the constraint is tangled_rope or snare from working scholar perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neologism_boundary_plasticity, empirical, 'Whether hybrid adequacy boundary is genuinely permissive or reconstructs suppression').

omega_variable(
    institutional_reading_determination,
    'Does the hybrid adequacy reading succeed because it represents a structural truth about how language must work (linguistic inevitability), or does it succeed because institutional actors (Renaissance academies, printing houses) chose it for pragmatic reasons and then naturalized the choice as inevitable?',
    'Historical counterfactual: what would have happened if strict Ciceronism had won institutional backing instead? Comparative analysis of other linguistic communities (Arabic, Hebrew, Sanskrit) managing the same classical-adequacy tension — do they all converge on hybrid adequacy, or do different institutional choices produce different outcomes?',
    'If linguistic inevitability: the hybrid reading''s authority is grounded in structural necessity. If institutional choice: the reading''s authority depends on the power configuration that backs it. Affects whether this constraint is fundamentally about language (mountain candidate) or about power over language (extraction mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_reading_determination, conceptual, 'Whether hybrid adequacy is linguistic necessity or institutional choice').

omega_variable(
    reading_coexistence_mechanism,
    'Can the hybrid adequacy reading and the strict Ciceronian reading genuinely coexist within a single institutional framework, or does institutional adoption of hybrid adequacy necessarily displace strict Ciceronism as a legitimate orthodox position?',
    'Institutional history of Renaissance academies and universities: which ones adopted hybrid adequacy? Which maintained strict Ciceronism? Did adoption of hybrid adequacy change the status of strict Ciceronism from ''the correct standard'' to ''a valid aesthetic choice''? Or did it become marginalized as pedantic?',
    'If coexistence: reading_relations should show coexists_with. If displacement: reading_relations should show influences (hybrid adequacy undermines strict Ciceronism''s claim to be THE orthodoxy). Affects the structural diagnosis of what the kernel (latin_correctness) actually means.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_mechanism, empirical, 'Whether sibling readings can coexist within one institutional framework').

omega_variable(
    reading_as_false_summit_candidate,
    'Does the hybrid adequacy reading itself risk becoming a false summit — naturalizing what is actually a power-laden institutional choice (who decides which neologisms count as valid?) as a linguistic necessity (language must adapt)?',
    'Close analysis of how the reading is invoked in actual practice: is ''adequacy'' used to justify innovation by powerful scholars while denying it to marginal ones? Do academy decisions on neologism approval correlate with political allegiance or professional patronage rather than linguistic principle?',
    'If false summit confirmed: the reading itself instantiates extraction (some scholars get to define adequacy; others are bound by those definitions). The tangled_rope classification would persist. If false summit not confirmed: the reading''s appeal to functional adequacy is genuine and represents a real reduction in extraction compared to strict Ciceronism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_false_summit_candidate, empirical, 'Whether hybrid adequacy reading itself naturalizes power asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_adequacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_hybrid, hybrid_adequacy_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(theater_mid_hybrid, hybrid_adequacy_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(theater_late_hybrid, hybrid_adequacy_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(extract_early_hybrid, hybrid_adequacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(extract_mid_hybrid, hybrid_adequacy_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(extract_late_hybrid, hybrid_adequacy_reading, base_extractiveness, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_adequacy_reading, identity_coordination).
narrative_ontology:affects_constraint(hybrid_adequacy_reading, textual_recovery_reading).
narrative_ontology:affects_constraint(hybrid_adequacy_reading, living_drift_reading).
narrative_ontology:affects_constraint(hybrid_adequacy_reading, renaissance_academy_authority).
narrative_ontology:affects_constraint(hybrid_adequacy_reading, mechanical_printing_vernacular_shift).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel generates multiple constraint stories, each one a reading instantiating a different institutional solution to the classical-adequacy tension. The hybrid adequacy reading is structurally distinct from strict Ciceronism (lower extractiveness by ~0.15–0.20) and from living drift (higher suppression due to classical constraint). Each reading has its own beneficiary/victim structure and its own classification profile. Network edges show downstream influences: the hybrid adequacy reading influences strict Ciceronism (undermines its claim to be THE orthodoxy) and influences living drift (constrains how far that reading can push innovation without losing classical legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_adequacy_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
