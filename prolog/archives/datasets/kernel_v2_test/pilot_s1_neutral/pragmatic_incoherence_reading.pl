% ============================================================================
% CONSTRAINT STORY: pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pragmatic_incoherence_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: pragmatic_incoherence_reading
 *   human_readable: Pragmatic Incoherence in Kami-Buddha Syncretism: Ontological Flexibility Without Resolution
 *   domain: religious_studies/japanese_history/metaphysics
 *
 * SUMMARY:
 *   For approximately 1,200 years in Japan, Buddhist and kami-worshipping
 *   (Shinto) frameworks coexisted without formal resolution of their
 *   metaphysical incompatibilities. This constraint story models the
 *   pragmatic incoherence reading — the interpretation that practitioners,
 *   institutional authorities, and state regulators navigated contradictory
 *   frameworks opportunistically, maintaining incoherence as official policy
 *   rather than resolving it toward fusion, partition, or dominance. The
 *   pragmatic incoherence reading differs structurally from two sibling
 *   readings: the syncretic fusion reading (apparent contradictions resolve
 *   into deeper coherence at levels neither tradition articulated) and the
 *   domain partition reading (kami and Buddha govern genuinely separate
 *   ontological domains, making contradiction apparent but not real). This
 *   story instantiates the pragmatic incoherence reading as a clean,
 *   ε-invariant constraint: multiple incompatible extractiveness structures
 *   operated simultaneously; ontological coherence is the victim; ritual
 *   flexibility and institutional benefit-collection are the beneficiaries.
 *   The constraint required active suppression of the impulse to resolve —
 *   any definitive answer to 'are kami and Buddha the same or different?'
 *   would collapse the arrangement's extractiveness structure.
 *
 * KEY AGENTS:
 *   - Village Practitioner (Ritual Performer): Powerless/identity_locked — embedded in community practice; bears full burden of navigating contradictions without systematic explanation; identity constituted through integrated Buddhist-kami participation
 *   - Village Priest (Multi-Tradition Authority): Moderate/constrained — coordinates local ritual needs across both traditions; benefits from expanded authority and dual revenue; constrained by obligation to maintain credibility in both systems without resolving their contradiction
 *   - Institutional Buddhism: Institutional/arbitrage — benefits from territorial expansion and revenue base through kami absorption; maintains pure doctrine for elite audiences; arbitrage capacity lets it exit any particular local arrangement
 *   - Shinto Institutional Authority: Institutional/arbitrage — benefits from maintaining kami cult authority without systematic theology; Buddhist frameworks legitimize kami cosmologically; arbitrage capacity enables selective invocation of Buddhist cosmology or kami-only framing
 *   - State Regulatory Authority: Powerful/constrained — coordinates social order without enforcing doctrinal coherence; benefits from avoiding costly unification; constrained by obligation to maintain legitimacy across both traditions
 *   - Ontological Coherence (Abstract Victim): Powerless/analytical — trapped by the constraint arrangement; cannot exit or organize because it is an abstract property, not an organized actor; experiences maximum extraction as coherence is actively suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pragmatic_incoherence_reading, 0.68).
domain_priors:suppression_score(pragmatic_incoherence_reading, 0.62).
domain_priors:theater_ratio(pragmatic_incoherence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pragmatic_incoherence_reading, tangled_rope).
narrative_ontology:human_readable(pragmatic_incoherence_reading, "Pragmatic Incoherence in Kami-Buddha Syncretism: Ontological Flexibility Without Resolution").
narrative_ontology:topic_domain(pragmatic_incoherence_reading, "religious_studies/japanese_history/metaphysics").

domain_priors:requires_active_enforcement(pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pragmatic_incoherence_reading, '6193c7ae-c5bc-4528-8e64-d6841f7b8e54').
narrative_ontology:cs_kernel_codification('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', distributed).
narrative_ontology:cs_authority_grounding('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', extraction).
narrative_ontology:cs_reading_relation('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', pragmatic_incoherence_reading__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', pragmatic_incoherence_reading__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', foundational, incoherence_is_irreducible).
narrative_ontology:cs_axiom_status(incoherence_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', incoherence_is_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', foundational, pragmatism_trumps_coherence).
narrative_ontology:cs_axiom_status(pragmatism_trumps_coherence, holdable).
narrative_ontology:cs_axiom_grounding('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', pragmatism_trumps_coherence, instrumental).
narrative_ontology:cs_reference_frame('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', pragmatic_coexistence_without_resolution).
narrative_ontology:cs_drift_state('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6193c7ae-c5bc-4528-8e64-d6841f7b8e54', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(pragmatic_incoherence_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, ritual_practitioners).
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, institutional_buddhism).
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, shinto_priesthood).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, ontological_coherence).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, doctrinal_consistency).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, systematic_theology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRACTITIONER (SNARE) — Embedded in community practice without access to doctrinal frameworks. Identity constituted through ritual participation. Cannot exit without abandoning social position and family continuity. Experiences the constraint as inescapable practical incoherence: perform both kami and Buddha rituals, navigate contradictory advice from different authorities, receive no systematic explanation. The extraction mechanism is the performative burden — the practitioner bears all cost of navigating incompatibility while authorities collect legitimacy from both traditions.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: VILLAGE PRIEST / MULTI-TRADITION AUTHORITY (TANGLED ROPE) — Serves both kami and Buddha functions, coordinating local ritual needs across incompatible frameworks. Benefits from expanded authority and revenue (both shrine and temple collections). Constrained by doctrinal inconsistency — must maintain credibility in both traditions without resolving their contradiction. The coordination function (integrating diverse local spiritual needs) coexists with asymmetric extraction (priest collects from both systems while practitioners bear coherence burden).
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL BUDDHISM (ROPE) — Benefits from syncretism without resolving doctrinal incompatibility. Expands territorial reach and revenue base through kami absorption. Maintains pure doctrinal tradition for elite audiences (monasteries, educated audiences) while tolerating incoherent folk practice. Experiences the arrangement as coordination — stretching Buddhism to accommodate local kami cults solves the problem of converting populations with existing spiritual frameworks. Arbitrage capacity lets Buddhism exit any particular local arrangement without doctrinal cost.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SHINTO INSTITUTIONAL AUTHORITY (ROPE) — Benefits from syncretism by maintaining kami cult authority without requiring systematic theology. Buddhist frameworks legitimize kami within cosmological hierarchy. Arbitrage: can selectively invoke Buddhist cosmology when advantageous, return to kami-only framing when defending autonomy. Experiences coordination function: Buddhist inclusion solves the problem of justifying kami worship to literate audiences. No extraction experienced because authority is not enforcing the incoherence — practitioners and village priests bear that burden.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE REGULATORY AUTHORITY (TANGLED ROPE) — Coordinates social order across multiple religious traditions without enforcing coherence. Stabilizes by making incoherence official policy: both kami and Buddha are legitimate; contradiction is acceptable if socially functional. Benefits from avoiding doctrinal enforcement (reduces resistance and unification costs). Constrained by the obligation to maintain legitimacy across both traditions without collapsing either. The extraction mechanism is deferred: state collects taxes/loyalty from both systems; practitioners and doctrinal integrity bear the cost.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DOCTRINAL COHERENCE AS ABSTRACT VICTIM (SNARE) — Ontological consistency and systematic theology are trapped in a constraint structure that actively prevents their realization. The arrangement extracts coherence from the theological tradition by making incoherence official, normalized, and morally neutral ('both are true'; 'contradiction is acceptable in practice'). Analytical perspective reveals that ontological integrity is the actual victim — the framework has been engineered to suppress the pressure to resolve incompatibility. No agent can organize on behalf of coherence because coherence has no constituency (it is an abstract property, not an organized actor).
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, ontological incoherence may appear as a natural law: plural traditions always contain incompatible metaphysics; attempting to unify them generates conflict; pragmatic coexistence without resolution is the inevitable solution. This perspective risks naturalizing what is actually a contingent institutional arrangement — the beneficiaries (institutional religion, state authority, flexible practitioners) have constructed and maintained the incoherence. The framework asserts that incompatibility is irreducible and therefore unchallengeable. However, the declared beneficiaries and extraction flow indicate this is a false summit.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pragmatic_incoherence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pragmatic_incoherence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pragmatic_incoherence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Substantial and rising. The constraint extracts ontological coherence from the theological tradition by normalizing incoherence. Practitioners bear the cognitive burden of navigating contradictions; village authorities bear the burden of maintaining dual credibility; institutional religions benefit from both doctrinal purity (for elites) and practical flexibility (for folk); state benefits from managing multiple traditions without costly unification. The trajectory (0.45 → 0.62 → 0.68) shows the extraction mechanism tightening as enforcement infrastructure matured: early Heian syncretism was pragmatic accommodation; medieval period saw formalization of coexistence frameworks (theater rises); by Edo period, the incoherence was enforced as official policy, and by Meiji it became a structural feature of Japanese modernity. Suppression (0.62): Moderate-high and rising. The primary suppressed pressure is the intellectual drive to resolve contradiction — practitioners and scholars who attempted coherent unification faced cultural resistance, institutional opposition, or were absorbed into a 'syncretic fusion' frame that denied the contradiction rather than resolving it. Rising suppression reflects hardening enforcement: Edo period saw explicit institutional policies protecting incoherence; Meiji saw state codification of kami-buddha coexistence as national policy. Theater ratio (0.55): Moderate and rising. Early syncretism had genuine coordination function (accommodating diverse populations, avoiding conflict). Over time, the theater increased as the original problem (religious diversity without conflict mechanism) became normalized and less pressing. By Edo period, much of the ritual performance was maintenance theater — continuing the arrangement because it worked, not because the underlying problem required it. Theater plateau at 0.55 rather than 0.70+ suggests real functional substrate remains (practitioners do gain ritual flexibility, state does coordinate diverse populations); constraint is not pure Piton but is increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark divergence across perspectives. The village practitioner experiences a snare (trapped by incoherence, no exit, maximum extraction). The village priest experiences a tangled rope (coordinates genuine ritual needs, benefits from dual authority, but extraction persists through suppression of coherence). Institutional religions experience rope (pure coordination benefit, no extraction experienced because authorities are not enforcing coherence). The state experiences tangled rope (coordinates order, collects legitimacy, but constrained by obligation to maintain both traditions). Doctrinal coherence experiences snare at abstract level (trapped, victimized, unable to organize). The civilizational analytical observer risks seeing a mountain (incoherence as natural law, irreducible, immutable) but the structural data reveals false summit: beneficiaries are identifiable, extraction is real, suppression is active.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from power, exit options, and beneficiary/victim status. Village practitioners: d ≈ 1.0 (full target) because they are powerless, identity_locked in the constraint, and bear coherence burden while authorities collect benefits. Village priests: d ≈ 0.65 (target with some agency) because they are moderate power, constrained exit, and experience both coordination benefit and extraction burden. Institutional religions: d ≈ 0.0 to -0.2 (full beneficiary) because they have institutional power, arbitrage exits, and collect doctrinal purity + revenue benefits. State: d ≈ 0.5-0.6 (symmetric or slight target) because it has powerful institutional position but is constrained by need to maintain both traditions; experiences coordination benefit (managing diversity) and extraction burden (suppression maintenance). Ontological coherence (abstract): d = 1.0 (full target, no exit, trapped) because it is the victim that cannot organize or resist.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate of kami-buddha syncretism was to accommodate multiple spiritual traditions within a unified state without enforcing doctrinal coherence or imposing destructive religious conflict. In early Heian period (t=0), this mandate was live and functional: Buddhism was expanding into Japan; kami worship was the indigenous tradition; accommodating both without resolving their metaphysical differences prevented the costly unification conflicts that plagued Europe. By Edo period (t=250), the mandate was increasingly institutionalized and formalized: the incoherence became official policy rather than pragmatic necessity. By Meiji (t=400), the mandate was arguably dead — the original problem (managing religious transition without conflict) had been solved, but the constraint persisted because institutional beneficiaries (religions, state authority, flexible practitioners) benefited from continued incoherence. The Meiji period's attempt to separate and hierarchize kami and Buddhism ('State Shinto' vs Buddhism) briefly threatened the constraint, but postwar Japan re-stabilized the pragmatic incoherence through constitutional religious freedom protections. The constraint persists today despite the founding problem being resolved, suggesting a piton trajectory (theater theater rising toward Piton classification) alongside tangled_rope core function. Mark mandatrophy_resolved as false: the problem of whether the mandate remains live (or has become dead theater) is unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_syncretic_fusion,
    'Is pragmatic incoherence (''we navigate contradictions without resolving them'') the core structure of kami-buddha syncretism, or is it a surface description obscuring deeper syncretic fusion (the ''syncretic_fusion_reading'') where apparent contradictions are actually coherent at a level neither tradition articulated?',
    'Textual analysis of medieval Japanese Buddhist-kami texts, lay theology tracts, and doctrinal commentaries that either (a) claim explicit fusion (fusion reading evidence) or (b) describe navigation of contradiction as the intended arrangement (incoherence reading evidence). Ethnographic documentation of how practitioners describe their own understanding: do they report experiencing contradiction as resolvable in principle, or as genuinely irreducible?',
    'If fusion reading is correct: this constraint reclassifies toward rope (coherent coordination) and the beneficiaries shift (coherence maintainers rather than extraction beneficiaries). If incoherence reading is correct: the snare and tangled_rope classifications stand; ontological consistency remains trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_syncretic_fusion, empirical, 'Whether kami-buddha syncretism is pragmatic incoherence or deeper syncretic fusion').

omega_variable(
    reading_vs_domain_partition,
    'Is this a case of pragmatic incoherence (multiple incompatible frameworks operating without resolution), or domain partition (domain_partition_reading: kami and Buddha govern genuinely separate ontological domains such that no contradiction exists — they are incommensurable but non-overlapping)?',
    'Analysis of actual practice domains: do kami and Buddha address the same events/problems (indicating genuine overlap and thus real incoherence), or do they address strictly separate domains (life-cycle rituals vs karmic destiny; local geography vs cosmic order; indicating successful partition)? Examination of ritual contexts where both traditions address the same phenomenon (e.g., a death, a harvest, a child''s birth) to determine whether they are treated as supplementary (no contradiction) or genuinely incompatible (contradiction suppressed).',
    'If domain partition is correct: this reading dissolves; there is no incoherence, only specialization. If pragmatic incoherence is correct: the snare classification confirms that ontological integrity is the victim — the arrangement actively prevents recognizing genuine mutual exclusivity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_domain_partition, empirical, 'Whether the constraint is pragmatic incoherence or successful domain partition').

omega_variable(
    extraction_mechanism_mechanism,
    'What is extracting what? Is the extraction flow primarily: (a) institutional religion extracting legitimacy from practitioners'' labor, (b) state authority extracting social order from religious incoherence, (c) ritual flexibility extracting theological coherence, or (d) some combination?',
    'Historical analysis of resource flows (money, labor, tax collection, authority recognition) during the syncretism period. Examination of what would happen if the incoherence were resolved in any single direction (kami-only, Buddha-only, syncretic fusion, partition): who would lose authority, revenue, and flexibility? The beneficiary group identifies the extraction direction.',
    'Clarifies which perspectives experience extraction vs coordination. If institutional religion benefits most: institutional perspectives should reclassify toward snare. If state benefits most: state perspective should show higher extraction. If the abstract property (coherence) is extracted: the abstract snare perspective confirms the constraint is engineering incoherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_mechanism, empirical, 'Identification of primary extraction flow in the pragmatic incoherence arrangement').

omega_variable(
    mandatrophy_status,
    'Does the founding mandate of kami-buddha syncretism (accommodate multiple traditions without enforcing coherence; preserve local spiritual autonomy while expanding state religious authority) remain live, or has it become a dead mandate maintained theatrically?',
    'Historical comparison: (1) during Heian period: was the mandate serving its intended function (accommodating diversity, preventing conflict, stabilizing state control)? (2) during later periods: does the constraint continue serving that function, or does it persist primarily through institutional inertia and power maintenance? (3) contemporary: would dismantling the official incoherence generate conflict, or would practitioners and institutions adapt cleanly to a resolved framework?',
    'If mandate is live: constraint is a functional tangled_rope. If mandate is dead: constraint may be a piton (degraded into theater). If mandate is contested: mark as unresolved mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_status, conceptual, 'Status of founding mandate for pragmatic incoherence arrangement').

omega_variable(
    identity_lock_vs_structural_trap,
    'Is the village practitioner trapped because of material/social barriers to religious switching (structural trap), or because their identity is constituted through Buddhist-kami practice as an integrated whole, making ''picking one'' psychologically unthinkable (identity_locked)?',
    'Ethnographic analysis: (1) practitioners who have shifted to Buddhist-only or kami-only practice — was the shift experienced as abandoning an identity or solving a practical problem? (2) practitioners who have left their home communities — do they continue practicing incoherence or do they restructure their practice? (3) doctrinal and ritual texts — do they frame the integration as identity-constituting or as pragmatically necessary?',
    'If identity_locked: exit_options for village practitioner should be identity_locked, not trapped. If trapped is correct: the exit barrier is structural (social cost, geographic isolation, economic dependency) not cognitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Whether village practitioner exit options are identity-locked or structurally trapped').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatic_incoherence_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pragma_theater_t0_heian, pragmatic_incoherence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pragma_theater_t250_edo, pragmatic_incoherence_reading, theater_ratio, 250, 0.48).
narrative_ontology:measurement(pragma_theater_t400_meiji, pragmatic_incoherence_reading, theater_ratio, 400, 0.55).

% Extraction over time
narrative_ontology:measurement(pragma_extract_t0_heian, pragmatic_incoherence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pragma_extract_t250_edo, pragmatic_incoherence_reading, base_extractiveness, 250, 0.62).
narrative_ontology:measurement(pragma_extract_t400_meiji, pragmatic_incoherence_reading, base_extractiveness, 400, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pragma_supp_t0_heian, pragmatic_incoherence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pragma_supp_t250_edo, pragmatic_incoherence_reading, suppression_requirement, 250, 0.6).
narrative_ontology:measurement(pragma_supp_t400_meiji, pragmatic_incoherence_reading, suppression_requirement, 400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(pragmatic_incoherence_reading, 0.12).
narrative_ontology:affects_constraint(pragmatic_incoherence_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(pragmatic_incoherence_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% The kernel kami_buddha_ontology has three structurally distinct readings, each with different ε values and classifications: (1) pragmatic_incoherence_reading (extractiveness 0.68, tangled_rope core) — arrangements navigate contradictions without resolving; incoherence is maintained. (2) syncretic_fusion_reading (lower ε expected) — apparent contradictions resolve into deeper coherence. (3) domain_partition_reading (near-zero ε expected) — kami and Buddha govern separate ontological domains. Each reading is a separate constraint story with its own perspectives, beneficiaries, victims, and measurements. They are linked by the kernel relation, not by observable variation — the ε-invariance principle applies: if changing the reading changes ε, we have different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
