% ============================================================================
% CONSTRAINT STORY: revolutionary_legitimacy_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_revolutionary_legitimacy_scaffold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: revolutionary_legitimacy_scaffold
 *   human_readable: Revolutionary Legitimacy Scaffold: Turkey's 1928 Alphabet Reform
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   Turkey's 1928 alphabet reform (Law 1353) is the canonical test case for
 *   whether a commitment system kernel can be installed without prior
 *   occupancy of the new reading. Atatürk's government replaced the Arabic
 *   script with Latin script within months via top-down legal imposition,
 *   with effectively zero prior practitioners of Latin-script Turkish. The
 *   reform succeeded in installing a new kernel despite violating apparent
 *   prerequisites of institutional change. This constraint exemplifies how
 *   revolutionary legitimacy operates as a temporary scaffold: high initial
 *   suppression and enforcement apparatus that declines as new cohorts
 *   naturally internalize the new reading. The structural data reveals
 *   whether this success was a violation of natural laws of language change
 *   (mountain view) or a achievement of state-enforced institutional
 *   transformation (scaffold view). The contrast between the trapped
 *   perspective of Arabic-literate elders (who experienced total literacy
 *   loss) and the organic perspective of youth who learned only Latin script
 *   demonstrates how a single institutional change instantiates different
 *   constraints across generational cohorts.
 *
 * KEY AGENTS:
 *   - Kemalist State Apparatus: Primary beneficiary (institutional/arbitrage) — consolidates state control over knowledge transmission, unifies literacy standard, enables modernization alignment with Western institutions. Has full exit arbitrage (can maintain parallel systems, adjust pace, or theoretically revert).
 *   - Arabic-Literate Population (Elders): Primary victim (powerless/trapped) — experiences total literacy loss within enforcement window. Cannot exit without accepting functional illiteracy in reformed state. Bears maximum extraction.
 *   - Religious Institutional Authority: Secondary victim (moderate/constrained) — experiences extraction of state recognition and institutional decoupling from knowledge transmission. Maintains alternative literacy system (Arabic for Quranic instruction) but loses state resources and authority claim.
 *   - Secular Modernization Coalition: Organized actors (organized/constrained) — youth cohorts, urban intellectuals, Western-aligned professionals. See the reform as temporary transition with sunset logic. Control the enforcement direction but constrained by administrative burden of managing transition.
 *   - Ottoman Script Continuity Claim: Institutional reading (institutional/arbitrage) — degraded from functional institutional status to vestigial cultural artifact. Maintains symbolic presence but zero state authority backing.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent political achievement as an immutable fact about language systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(revolutionary_legitimacy_scaffold, 0.52).
domain_priors:suppression_score(revolutionary_legitimacy_scaffold, 0.68).
domain_priors:theater_ratio(revolutionary_legitimacy_scaffold, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(revolutionary_legitimacy_scaffold, extractiveness, 0.52).
narrative_ontology:constraint_metric(revolutionary_legitimacy_scaffold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(revolutionary_legitimacy_scaffold, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(revolutionary_legitimacy_scaffold, scaffold).
narrative_ontology:human_readable(revolutionary_legitimacy_scaffold, "Revolutionary Legitimacy Scaffold: Turkey's 1928 Alphabet Reform").
narrative_ontology:topic_domain(revolutionary_legitimacy_scaffold, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(revolutionary_legitimacy_scaffold).
narrative_ontology:has_sunset_clause(revolutionary_legitimacy_scaffold).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(revolutionary_legitimacy_scaffold, formalized).
narrative_ontology:cs_authority_grounding(revolutionary_legitimacy_scaffold, extraction).
narrative_ontology:cs_interpretation_layer_present(revolutionary_legitimacy_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(revolutionary_legitimacy_scaffold, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(revolutionary_legitimacy_scaffold, bureaucratic_modernization).
narrative_ontology:constraint_victim(revolutionary_legitimacy_scaffold, arabic_literate_population).
narrative_ontology:constraint_victim(revolutionary_legitimacy_scaffold, religious_institutional_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LITERATE ELDER (SNARE) — Fluent in Arabic script; now faces total literacy loss overnight. Cannot exit the new reading without accepting functional illiteracy in a reformed state. Suppression is absolute: all official documents, education, and state communication shift to Latin script. No intermediate period, no parallel validity. The constraint traps the agent in immobility — resistance means becoming a non-participant in the reformed institutional order. Maximum extraction from the trapped perspective.
constraint_indexing:constraint_classification(revolutionary_legitimacy_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS INSTITUTION / ARABIC LITERACY PRESERVATIONISTS (TANGLED ROPE) — Possess genuine coordination function (Arabic script linked to Quranic interpretation, Islamic learning tradition) while experiencing extraction (state monopoly on literacy pedagogy shifts to secular Latin script; religious authority decoupled from institutional knowledge-bearing). Constrained exit: can maintain Arabic literacy internally but loses state recognition and resource allocation. Both coordination and extraction present — the constraint enforces a new reading while suppressing the institutional basis of the old one.
constraint_indexing:constraint_classification(revolutionary_legitimacy_scaffold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KEMALIST STATE APPARATUS (ROPE) — Benefits from rapid script reform as coordination mechanism: unifies the nation around a single literacy standard, breaks linguistic continuity with Ottoman precedent, aligns Turkey with European/Latin orthographies, and consolidates state control over knowledge transmission. Experiences the constraint as pure coordination from their position — they are the reading authority installing the new kernel. Zero suppression from their perspective; full arbitrage (they can always revert, maintain parallel systems, or adjust pace). The reform solves a real coordination problem for modernizing state institutions.
constraint_indexing:constraint_classification(revolutionary_legitimacy_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR MODERNIZATION COALITION (SCAFFOLD) — Organized actors (urban intellectuals, younger cohorts, secular professionals, Western-aligned elites) see the script reform as a temporary enforced transition to a new literacy regime. Low effective extraction despite high suppression because the coalition has agency in the transition direction and perceives a sunset: within a generation, native speakers will have learned only Latin script, and the constraint's enforcing apparatus becomes unnecessary — literacy is 'naturally' Latin. Theater ≤ 0.70; sunset clause operational (enforcement reduces as new cohorts naturalize the reading). Constrained exit because the coalition must manage the enforcement apparatus and absorb costs of transition, but they control the direction and perceive clear endstate.
constraint_indexing:constraint_classification(revolutionary_legitimacy_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OTTOMAN SCRIPT CONTINUITY (PITON — DEGRADED READING) — From a civilizational perspective, Arabic script represented continuity with Ottoman institutional legitimacy and Islamic learning authority. The Kemalist reform degrades this reading's institutional occupancy: continuity becomes vestigial, maintained only through private practice and religious instruction. The reading persists in cultural memory and diaspora but has no state authority backing. Theater ratio is high (Ottoman continuity appeals survive in rhetoric despite institutional death) but effectiveness has collapsed. The piton classification marks the transition from living institutional reading to historical artifact.
constraint_indexing:constraint_classification(revolutionary_legitimacy_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, this constraint risks appearing as a natural law: literacy systems are immutable once established; populations cannot rapidly shift script without decades of disruption; prior occupancy of a reading is a prerequisite for new kernel installation. The 1928 reform appears to violate this 'law' by succeeding despite zero prior Latin-script occupancy and compressed timeline. However, the structural data reveals a false summit: the constraint's success relied entirely on state coercion (suppression = 0.68), not on natural inevitability. The mountain classification here naturalizes a contingent political achievement as an immutable fact about language.
constraint_indexing:constraint_classification(revolutionary_legitimacy_scaffold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(revolutionary_legitimacy_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(revolutionary_legitimacy_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(revolutionary_legitimacy_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(revolutionary_legitimacy_scaffold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(revolutionary_legitimacy_scaffold, TR),
    TR >= 0.70.

:- end_tests(revolutionary_legitimacy_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, declining over interval. Peaks at t=2 (0.52) when enforcement is maximum and no cohort has matured in Latin script. Declines to 0.38 by t=10 as new literacy becomes naturalized and the enforcement apparatus becomes unnecessary. The extraction is front-loaded (suppression of alternatives, state monopolization of literacy) but not sustained indefinitely — this trajectory is diagnostic of a genuine scaffold, not a persistent snare. Theater ratio (0.55): Moderate-declining. Initial theater (0.68 at enforcement) reflects the performative aspects of the reform decree and mandatory implementation. Declines to 0.42 as the literacy transition becomes functionally transparent — no performance necessary once Latin script is the population's native writing system. The decline in theater is the signal that the constraint is genuinely sunsetting. Suppression (0.68): High, structural. Includes legal prohibition of Arabic-script official documents, educational system shift to Latin literacy, resource denial for Arabic-script pedagogy, career barriers for non-Latin-literate professionals. This is state-imposed suppression, not internalized barrier — the mechanism is coercive, not cognitive. Claimed type (Scaffold): Justified by the has_sunset_clause (true), the declining theater_ratio over time, and the clear endstate (when new generation matures in Latin-only literacy, enforcement can reduce). The constraint is explicitly temporary in structure, though it effects permanent institutional change.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The state architect sees pure coordination (Rope) — unifying a literacy standard solves real institutional problems. The beneficiary modernizers see temporary transition (Scaffold) — painful but directed, with a clear sunset. The trapped elder sees irreversible extraction (Snare) — literacy loss, social death, immobility. The religious institution sees hybrid coordination-extraction (Tangled Rope) — the constraint both enforces a new reading AND suppresses their institutional basis. The Ottoman script tradition sees its own degradation (Piton) — still invoked in rhetoric but institutionally dead. The analytical observer risks naturalizing this as inevitable (Mountain) — but the structural data reveals the 'inevitability' is entirely dependent on state coercion. No single classification is correct because each observer occupies a genuinely different structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation follows from beneficiary/victim declarations and exit options. Kemalist state apparatus: declared beneficiary with arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ (net beneficiary). Arabic-literate population: declared victim with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum χ (maximum target). Religious institution: declared victim with constrained exit → d ≈ 0.80 → f(d) ≈ 1.15 → high χ (target but some exit agency). Organized modernizers: organized power with constrained exit, declared beneficiary → d ≈ 0.40 → f(d) ≈ 0.40 → moderate positive χ (beneficiary with some burden). The scope modifier σ(S) applies national scope (1.0) for all perspectives except the analytical observer (global/universal, 1.0). No directionality overrides needed; the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY — extractiveness (0.52) is high enough to trigger the mandatrophy gate (> 0.46) but is not resolved (mandatrophy_resolved: false). The mandatrophy here is structural: Is this constraint a temporary scaffold with genuine sunset logic, or is it a persistent snare disguised as temporary? The declining theater_ratio (0.68 → 0.42) and declining extractiveness (0.52 → 0.38) suggest scaffold (mandatrophy resolved toward temporary coordination problem). However, the omega variables flag irreducible uncertainties: (1) Does the religious reading genuinely persist in parallel or does it collapse with state suppression? (2) Is the state enforcement contingent on continuous power or does the new kernel become self-sustaining? The mandatrophy is not resolved because the constraint's true duration and endstate depend on empirical facts about the stability of the installed kernel and the institutional trajectory of religious authority post-1928. The temporal measurements show the trajectory consistent with scaffold logic; the omega variables flag where that interpretation could be wrong.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    m_set_gap_kernel_installation,
    'Can a commitment system kernel be installed without prior occupancy of the new reading? Does zero prior practitioner density prevent ex nihilo kernel installation?',
    'Comparison of 1928 Turkey with other script reforms (Indonesia''s Latin romanization, Vietnam''s quốc ngữ adoption, Korea''s hangul expansion). Control: were there pre-existing practitioners? Success metric: literacy penetration curves, intergenerational continuity, institutional stability. If Turkey succeeded with zero prior occupancy while others required seeding practitioners, M-set gap is empirically real.',
    'If M-set gap is real: revolutionary legitimacy requires state enforcement capacity, not prior reading occupancy. Scaffold classification is correct (temporary enforcement with natural sunset). If M-set gap is myth: prior occupancy was latent (Western-aligned intellectuals, bureaucratic examples, orthographic proposals). Classification shifts toward rope (coordination emerges from pre-existing consensus). If reform failed hidden but succeeded performatively: theater is actually higher, piton classification gains weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(m_set_gap_kernel_installation, empirical, 'Can commitment system kernels be installed without prior reading occupancy?').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.68) structural (state coercion, legal prohibition, resource denial) or internalized (agents come to see Latin script as inevitable, natural, superior)?',
    'Linguistic anthropology of script transition: diary records, educational data, resistance patterns. If suppression decays as new cohorts mature into Latin literacy, it was structural (scaffold). If suppression persists as internalized resistance or periodic revival movements, significant internalized component present (suggest raising suppression estimate or noting heterogeneity).',
    'If purely structural: scaffold endstate is correct, enforcement reduces as new generation matures. If internalized: some agents carry suppression beyond enforcement window; constraint''s true duration extends beyond visible authority decline. Classification remains scaffold but with longer effective sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs internalized suppression in script reform').

omega_variable(
    revolutionary_reading_authority_legitimacy,
    'What legitimacy grounds the Kemalist state''s authority to install a new reading without prior consensus? Is this authority grounded in expertise (linguistic reform improves efficiency), lineage (modernization as national continuity), extraction (script reform concentrates state control), or distributed denial?',
    'Analysis of Kemalist justification rhetoric: efficiency claims, national unity framing, Europeanization arguments, religious decoupling. Comparison with public reception: did population accept the reading as legitimate or as coercive? If majority internalize Latinity as beneficial within a generation, authority is sustained by perceived expertise/lineage (rope logic). If majority resist but comply due to enforcement, authority is sustained by state capacity (tangled_rope/snare logic).',
    'If expertise/lineage: the constraint functions as legitimate revolutionary modernization (scaffold with rope components). If extraction: the constraint is revolutionary power consolidation (scaffold masking snare). Determines whether this is a case study in state capacity or state overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolutionary_reading_authority_legitimacy, conceptual, 'Legitimacy ground for revolutionary reading installation authority').

omega_variable(
    kernel_codification_stability,
    'Has the Latin-script kernel crystallized into a stable, revisable formal codification, or does it remain dependent on continuous state enforcement? Is the reading fixed or still enforced?',
    'Assess degree of institutional embedding: Are Latin-script rules written into law? Can they be revised through normal legislative process or do they require extraordinary enforcement? Do subsequent governments treat the reform as reversible or immutable? If revisable through law: kernel is formalized and stabilized. If enforced through administrative practice and backed by coercive power: kernel is implicit/implicit and contingent.',
    'If stable: the constraint''s sunset is genuine—enforcement can reduce. If contingent: the constraint risks reversion if enforcement resources decline or state capacity weakens. Affects long-term classification trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_codification_stability, empirical, 'Stability and reversibility of the installed Latin script kernel').

omega_variable(
    religious_institutional_reading_suppression,
    'Does the suppression of Arabic script constitute suppression of the religious reading itself, or do they remain independent? Can Islamic learning tradition persist in parallel with Latin state literacy?',
    'Historical analysis of Quranic instruction, Islamic education, religious authority post-1928. If Arabic literacy (for Quran) continues via mosque instruction while state literacy is Latin: readings are decoupled and suppression of state reading does not suppress religious practice. If Arabic literacy collapse means religious practice collapse: readings are fused and suppression is total.',
    'If decoupled: religious institution experiences suppression of state literacy but not suppression of religious practice—classification may shift toward constrained rather than trapped for religious actors. If fused: suppression of script means suppression of religious authority—classification remains snare/tangled_rope. Affects victim severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_institutional_reading_suppression, empirical, 'Independence of religious reading from state script reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(revolutionary_legitimacy_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(revleg_theater_t0, revolutionary_legitimacy_scaffold, theater_ratio, 0, 0.68).
narrative_ontology:measurement(revleg_theater_t2, revolutionary_legitimacy_scaffold, theater_ratio, 2, 0.6).
narrative_ontology:measurement(revleg_theater_t5, revolutionary_legitimacy_scaffold, theater_ratio, 5, 0.53).
narrative_ontology:measurement(revleg_theater_t10, revolutionary_legitimacy_scaffold, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(revleg_extract_t0, revolutionary_legitimacy_scaffold, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(revleg_extract_t2, revolutionary_legitimacy_scaffold, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(revleg_extract_t5, revolutionary_legitimacy_scaffold, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(revleg_extract_t10, revolutionary_legitimacy_scaffold, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(revolutionary_legitimacy_scaffold, information_standard).
narrative_ontology:boltzmann_floor_override(revolutionary_legitimacy_scaffold, 0.05).
narrative_ontology:affects_constraint(revolutionary_legitimacy_scaffold, ottoman_script_continuity_degradation).
narrative_ontology:affects_constraint(revolutionary_legitimacy_scaffold, religious_authority_state_decoupling).
narrative_ontology:affects_constraint(revolutionary_legitimacy_scaffold, modernization_legitimacy_constraint).

% DUAL FORMULATION NOTE:
% The revolutionary legitimacy scaffold is decomposed from a larger family of constraints dealing with kernel installation and reading replacement. The Ottoman script continuity reading is a sibling story with ε≈0.35 (degraded institutional reading losing authority). The religious authority decoupling is a separate constraint (ε≈0.48) focused on the institutional separation mechanism. The modernization legitimacy constraint (ε≈0.42) covers the broader state-building narrative. All three stories share the 1928 reform as a structural event but analyze different aspects: this story (revolutionary_legitimacy_scaffold) focuses on the kernel installation mechanism and the temporary scaffold structure; the others focus on the victimization trajectories and institutional decoupling. Network edges link all three; the primary story (this one) affects the degradation stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
