% ============================================================================
% CONSTRAINT STORY: unified_manifestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unified_manifestation_reading, []).

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
 *   constraint_id: unified_manifestation_reading
 *   human_readable: Honji Suijaku (Original Ground, Manifest Traces) — Kami as Buddha Manifestations
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   The honji suijaku doctrine (original ground, manifest traces) represents
 *   one coherent reading of how kami and buddhas relate ontologically. This
 *   reading claims that kami are manifestations or traces (suijaku) of buddha
 *   original ground (honji) — buddhas or bodhisattvas themselves taking kami
 *   form to benefit Japanese sentient beings. Under this reading, kami have
 *   no autonomous theological existence; they are entirely dependent on
 *   buddha-nature for their being and legitimacy. This is a constructive
 *   answer to the 'what is the relationship between kami and buddhas?'
 *   question, but it is one answer among several structurally distinct
 *   alternatives (see sibling readings). The constraint's history spans
 *   approximately 600 years from early doctrinal formulation (~900 CE,
 *   systematized by Kobo Daishi traditions) through Meiji institutional
 *   separation (1868). The measurement interval models the doctrine's
 *   extractiveness trajectory: beginning with relatively low extractiveness
 *   (primarily a coordination mechanism for integrating new and old
 *   traditions), rising steadily as the doctrine becomes institutionalized
 *   and used to justify Buddhist authority over kami worship, peaking
 *   post-Meiji when the state invalidated the doctrine but communities
 *   continued to practice it through inertia. The theater ratio similarly
 *   rises as the doctrine's explanatory power declines (post-Meiji) while its
 *   narrative persistence increases.
 *
 * KEY AGENTS:
 *   - Buddhist Institutional Authority (institutional/arbitrage): Primary beneficiary — consolidates religious authority across Japanese landscape via ontological hierarchy that makes kami subordinate
 *   - Kami (as ontological dependents) (powerless/identity_locked): Primary victim — lose autonomous theological status; identity constituted through their dependence on buddha-nature; cannot exit without abandoning entire spiritual cosmos
 *   - Village Shrine Communities (moderate/constrained): Secondary victims/partial beneficiaries — benefit from integration into larger Buddhist cosmological order but experience suppression of kami-specific theology
 *   - Indigenous Shinto Theological Independence (powerless/trapped): Victim (abstract) — native religious framework loses capacity to assert autonomous metaphysical claims
 *   - Syncretic Integrated Cosmology (organized/constrained): Beneficiary (organizational) — coherent unified worldview that explains kami-buddha coexistence; requires active maintenance
 *   - Analytical Observer (analytical/analytical): Sees the framework as either ontological necessity (mountain) or contingent institutional construction (tangled rope) depending on epistemic premises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unified_manifestation_reading, 0.38).
domain_priors:suppression_score(unified_manifestation_reading, 0.52).
domain_priors:theater_ratio(unified_manifestation_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unified_manifestation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(unified_manifestation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(unified_manifestation_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unified_manifestation_reading, tangled_rope).
narrative_ontology:human_readable(unified_manifestation_reading, "Honji Suijaku (Original Ground, Manifest Traces) — Kami as Buddha Manifestations").
narrative_ontology:topic_domain(unified_manifestation_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(unified_manifestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(unified_manifestation_reading, formalized).
narrative_ontology:cs_authority_grounding(unified_manifestation_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(unified_manifestation_reading).
narrative_ontology:cs_kernel_id(unified_manifestation_reading, kami_buddha_ontology).
narrative_ontology:cs_reading_relation(unified_manifestation_reading, domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation(unified_manifestation_reading, pragmatic_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom(unified_manifestation_reading, foundational, buddha_nature_ontological_priority).
narrative_ontology:cs_axiom_status(buddha_nature_ontological_priority, holdable).
narrative_ontology:cs_axiom_grounding(unified_manifestation_reading, buddha_nature_ontological_priority, deontological).
narrative_ontology:cs_axiom(unified_manifestation_reading, foundational, kami_dependence_derivative_status).
narrative_ontology:cs_axiom_status(kami_dependence_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding(unified_manifestation_reading, kami_dependence_derivative_status, deontological).
narrative_ontology:cs_reference_frame(unified_manifestation_reading, buddha_nature_foundational_primacy).
narrative_ontology:cs_drift_state(unified_manifestation_reading, meiji_state_separation, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unified_manifestation_reading, buddhist_institutional_authority).
narrative_ontology:constraint_beneficiary(unified_manifestation_reading, syncretic_integrated_cosmology).
narrative_ontology:constraint_victim(unified_manifestation_reading, indigenous_kami_autonomy).
narrative_ontology:constraint_victim(unified_manifestation_reading, native_shinto_theological_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KAMI AS TRAPPED MANIFESTATIONS (SNARE) — In the honji suijaku framework, kami have no autonomous theological existence; they are entirely derivative traces of buddha original ground. A local kami worshipper's spiritual identity is constituted through this dependence — the kami they revere is both real and fundamentally not-itself, a mask worn by a buddha. Exit would require abandoning the entire spiritual cosmos one was raised in. Maximum extraction: kami lose independent status yet must justify their existence by reference to buddha superiority.
constraint_indexing:constraint_classification(unified_manifestation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: VILLAGE SHRINE COMMUNITIES (TANGLED ROPE) — Local shrine communities benefit from theological integration (participation in wider Buddhist cosmological order, access to Buddhist institutional resources, legitimacy through association). But they also bear costs: subordination of kami theology, pressure to adopt Buddhist ritual frameworks, gradual erosion of native-specific mythological content. Constrained by career/social dependencies on Buddhist-dominated institutional structures; benefit from coordination with wider religious authority. Moderate extraction with genuine coordination benefit.
constraint_indexing:constraint_classification(unified_manifestation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUDDHIST INSTITUTIONAL APPARATUS (ROPE) — Honji suijaku doctrine enables Buddhism to consolidate authority over the entire Japanese religious landscape without destroying local practices. Pure coordination function: Buddhist temples become managers of kami shrines; Buddhist priests perform kami rituals; a unified bureaucratic apparatus serves both traditions. The doctrine solves the coordination problem of integrating an established indigenous system with an incoming universal religion. Net beneficiary with genuine coordination benefit.
constraint_indexing:constraint_classification(unified_manifestation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOCTRINAL APPARATUS POST-MEIJI (PITON) — After the Meiji Restoration (1868), the Japanese state deliberately separated Buddhism and Shinto, invalidating honji suijaku as official doctrine. Yet the integrated cosmology persists in popular practice and folk theology through institutional inertia. The doctrine now operates primarily performatively — a narrative frame for why shrine worship and Buddhist practice coexist, maintained because it's embedded in centuries of practice, not because it functions as active authority. Theater ratio is high (0.65+) because the framework's explanatory power has been architecturally undermined.
constraint_indexing:constraint_classification(unified_manifestation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / ONTOLOGICAL PRIORITY (MOUNTAIN) — From a rigorous Buddhist metaphysical perspective, if buddhas represent ultimate reality (Buddha-nature, emptiness, universal consciousness) and kami are presented as traces of buddha-ground, then kami cannot exist as independent beings within that framework — it is logically impossible for contingent manifestations to have autonomous existence. The constraint appears as an immutable feature of Buddhist ontology itself. However, the beneficiary/victim structure contradicts this classification, revealing it as a false summit.
constraint_indexing:constraint_classification(unified_manifestation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SYNCRETIC INTEGRATED COMMUNITIES (TANGLED ROPE) — Communities that genuinely internalize the unified cosmos (kami ARE buddha manifestations, this is not subordination but completion) benefit from theological coherence and unification with larger Buddhist traditions. But they also experience extraction: their kami-specific theological discourse becomes subsumed into Buddhist categories; their local myths become examples of universal Buddhist principles rather than autonomous narratives. Organized within regions where honji suijaku is dominant; face constraint if they want to assert kami autonomy.
constraint_indexing:constraint_classification(unified_manifestation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unified_manifestation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unified_manifestation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unified_manifestation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(unified_manifestation_reading, TR),
    TR >= 0.70.

:- end_tests(unified_manifestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38): Moderate. The honji suijaku doctrine functions as a genuine coordination mechanism (it solves the problem of integrating Buddhism into Japan without displacing kami worship), producing real coordination benefit for Buddhist institutions and participating communities. However, the coordination is asymmetrical: Buddhist authority gains the capacity to manage and subsume kami theology within a larger unified framework. The extraction is not as severe as a pure Snare (which would lack coordination function) because the integrated cosmology genuinely benefits participating communities — they gain theological coherence and access to Buddhist institutional resources. Base extractiveness began lower (0.22) when the doctrine emerged as primarily a coordination solution; it rises over time as it becomes institutionalized and used to justify hierarchy and suppression of kami-specific discourse. SUPPRESSION (0.52): Moderate-high. The doctrine suppresses kami theological autonomy directly — under this reading, kami cannot assert independent metaphysical claims because their existence is derived and dependent. Suppression mechanisms include institutional enforcement (Buddhist temples managing shrines), doctrinal enforcement (claims that kami are buddha-manifestations cannot be challenged without abandoning the entire framework), and historical enforcement (Meiji state separation that invalidated the doctrine as policy, though communities continued the practice). The suppression is not total because kami worship persists and communities maintain the practice despite official delegitimation. THEATER RATIO (0.65): Moderate-high. Honji suijaku doctrine initially functioned explanatorily — it genuinely answered the question 'how do kami and buddhas coexist?' Post-Meiji, the doctrine's institutional basis was demolished by state decree, but the practice persisted through institutional and cultural inertia. The framework now operates primarily as narrative performance: a story explaining why shrine and temple coexist in the same communities, maintained because it is embedded in centuries of practice and cultural identity, not because it actively explains or justifies anything within contemporary institutional structures. The rise in theater ratio from 0.35 to 0.65 reflects this degradation. CLAIMED TYPE (tangled_rope): The constraint exhibits both coordination function (integrating kami and Buddha traditions into unified cosmology) and extraction (suppression of kami autonomy, Buddhist institutional authority over kami theology). The beneficiary/victim structure is clear: Buddhist institutions and syncretic communities benefit; kami (as theological entities) and native Shinto independence are victimized. Active enforcement is present (institutional machinery of temples managing shrines, doctrinal requirements that kami worship be justified through Buddhist cosmology). This satisfies the tangled rope gates.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme — the same constraint appears as Snare to the kami (ontologically trapped), as Rope to Buddhist institutions (pure coordination benefit with no extraction cost from their perspective), as Tangled Rope to village communities (mixed benefit and extraction), as Piton to the post-Meiji doctrinal apparatus (degraded and performative), as Mountain to the analytical observer who accepts Buddhist ontological premises (ontologically necessary), and as Tangled Rope to syncretic communities who genuinely internalize the integration. The gap reveals that the classification is entirely perspectival: from the kami's structural position, they are trapped in derivative existence; from the Buddhist institution's position, the constraint solves a coordination problem elegantly; from the analytical position, it appears to be either metaphysical necessity (mountain) or contingent theological construction (tangled rope) depending on whether one accepts the Buddhist premise that kami-nature is indeed derivable from buddha-nature. The analytical observer's mountain classification is the crux of the false summit question: does the honji suijaku doctrine describe a genuine metaphysical hierarchy (mountain) or naturalize a contingent institutional arrangement (false summit → tangled rope)? The beneficiary/victim structure strongly suggests false summit: if the doctrine is metaphysically necessary, why do Buddhist institutions need to enforce it, and why does it function as a mechanism for consolidating authority?
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional authority: Beneficiary + arbitrage exit → low d → negative χ. They experience the constraint as coordination benefit with no meaningful extraction cost to themselves. The doctrine allows them to expand authority without destroying existing practices. Kami (as theological entities): Victim + identity_locked exit → high d → high χ. They are trapped in derivative existence, and this trap is constituted through the entire cosmological framework they inhabit. Exiting would require abandoning not just the doctrine but the entire spiritual universe. Village shrine communities: Both beneficiary (access to Buddhist institutional resources) and victim (suppression of kami-specific theology) + constrained exit → moderate d. They have some agency to maintain kami-specific practices despite suppression, but face significant costs (career risk, social stigma, institutional pressure) for asserting kami autonomy. Syncretic communities: Beneficiary (genuine theological coherence) + constrained exit → low-to-moderate d. They actively choose the framework, but are constrained by cultural embeddedness and the difficulty of asserting alternative cosmologies. Analytical observer: Neither beneficiary nor victim, but positioned to see the structure → canonical analytical d. The observer's challenge is that accepting the Buddhist premise (buddha-nature is ultimate) makes the constraint appear as mountain; rejecting it makes the constraint appear as an institutional construction (tangled rope).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    buddha_nature_identity,
    'Is the ''buddha original ground'' that kami manifest actually identical with buddhas themselves, or is it a more abstract universal principle that buddhas merely exemplify?',
    'Textual analysis of honji suijaku sutras and commentaries; comparison with pure Buddha-nature doctrine across Buddhist schools; examination of whether kami can be said to manifest Buddha-nature without being traces of specific buddhas.',
    'If identical with buddhas: the constraint is ontological hierarchy (current classification). If abstract universal: kami and buddhas are co-manifestations of a third principle (reclassifies to symmetric coordination, ε drops ~0.15). If neither: the doctrine is conceptually incoherent (ω-driven omega for type stability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(buddha_nature_identity, conceptual, 'Whether buddha-ground is identical with buddhas or a more abstract principle').

omega_variable(
    historical_directional_causality,
    'Did honji suijaku develop as Buddhist assimilation of indigenous kami worship (top-down imposition), or as kami theology incorporating Buddhist cosmology (bottom-up adoption)?',
    'Historical textual archaeology; dating of earliest honji suijaku doctrinal texts vs. evidence of syncretism in practice; examination of whether the doctrine was imposed by institutional authority or emerged from communities.',
    'If top-down imposition: extraction mechanism is clearer (Buddhist authority imposes ontological hierarchy on existing tradition). If bottom-up adoption: communities actively chose integration, modifying extraction classification. If mixed/dialectical: perspectival gap widens between institutional and community readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_directional_causality, empirical, 'Direction of historical influence: Buddhist to kami or kami to Buddhist').

omega_variable(
    kami_autonomy_preservation,
    'Within the honji suijaku framework, do kami retain any theological characteristics that cannot be reduced to buddha attributes, or are all kami properties derivable from buddha-nature?',
    'Theological analysis of kami-specific virtues, domains, and mythological narratives; examination of whether kami are treated as identical with associated buddhas or as distinct persons with derivative status; comparison with other hierarchical theological systems (e.g., Christian angelology).',
    'If kami retain irreducible autonomous attributes: the framework permits genuine coordination (shifts toward rope). If fully reducible: pure subordination structure (confirms snare from kami perspective). If selectively preserved: extractive compromise (confirms tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_autonomy_preservation, conceptual, 'Whether kami have irreducible theological autonomy within honji suijaku').

omega_variable(
    meiji_separation_validity,
    'Did the Meiji state''s separation of Buddhism and Shinto invalidate honji suijaku as a theological claim, or merely as state policy, leaving the doctrine''s truth-value independent of political reversal?',
    'Examination of whether Buddhist and Shinto practitioners continued to teach and believe honji suijaku after 1868; analysis of whether the doctrine''s coherence depends on state institutional support; comparison with other doctrinally stable frameworks that survived institutional separation.',
    'If validity tied to state support: the doctrine''s classification shifts when state support ends (piton confirmed). If intellectually independent: the post-Meiji framework is doctrinally unchanged but politically delegitimized (reclassifies to institutional extraction). If practitioners split: bifurcation into separate readings based on whether one accepts Meiji separation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_separation_validity, empirical, 'Whether honji suijaku''s theological validity survived Meiji institutional separation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unified_manifestation_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unif_tr_t0, unified_manifestation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(unif_tr_t300, unified_manifestation_reading, theater_ratio, 300, 0.52).
narrative_ontology:measurement(unif_tr_t600, unified_manifestation_reading, theater_ratio, 600, 0.65).

% Extraction over time
narrative_ontology:measurement(unif_be_t0, unified_manifestation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(unif_be_t300, unified_manifestation_reading, base_extractiveness, 300, 0.38).
narrative_ontology:measurement(unif_be_t600, unified_manifestation_reading, base_extractiveness, 600, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unified_manifestation_reading, identity_coordination).
narrative_ontology:affects_constraint(unified_manifestation_reading, domain_partition_reading).
narrative_ontology:affects_constraint(unified_manifestation_reading, pragmatic_accommodation_reading).
narrative_ontology:affects_constraint(unified_manifestation_reading, kami_worship_ritual_subordination).
narrative_ontology:affects_constraint(unified_manifestation_reading, meiji_state_separation_doctrine).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel decomposes into three constraint stories, one per reading. Each reading instantiates a different constraint with different ε values and beneficiary/victim structures: unified_manifestation_reading (honji suijaku, ε=0.38, tangled_rope), domain_partition_reading (kami and buddhas in separate domains, ε~0.25, rope), pragmatic_accommodation_reading (functional coordination without metaphysical claim, ε~0.20, rope). The unified reading has the highest extractiveness because it makes an ontological subordination claim that can be enforced institutionally. The domain partition and pragmatic readings avoid this extraction because they do not make hierarchical metaphysical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
