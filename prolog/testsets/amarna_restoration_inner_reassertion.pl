% ============================================================================
% CONSTRAINT STORY: amarna_restoration_inner_reassertion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amarna_restoration_inner_reassertion, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: amarna_restoration_inner_reassertion
 *   human_readable: Post-Amarna Restoration as Inner-Container Reassertion
 *   domain: ancient_religion/restoration_pattern
 *
 * SUMMARY:
 *   After Akhenaten's death circa 1336 BCE, the Egyptian political and
 *   religious establishment orchestrated a systematic reversal of the Amarna
 *   theological experiment. Tutankhaten (later Tutankhamun) ascended to power
 *   as a minor and was controlled by regency figures who orchestrated rapid
 *   return to pre-Amarna religious practice. The Restoration Stela, inscribed
 *   by Tutankhamun (likely under priestly supervision), framed Akhenaten's
 *   reign as a catastrophic departure from Ma'at (cosmic order) that had
 *   contaminated temples, disrupted ritual, and provoked divine abandonment.
 *   Tutankhamun reopened temples, restored Amun worship, and explicitly
 *   condemned the Atenist heresy. Horemheb, who succeeded to power after
 *   Tutankhamun, systematized the restoration through damnatio memoriae: he
 *   destroyed Akhenaten's monuments, removed his name from official
 *   succession lists, and retro-dated his own reign from Amenhotep III,
 *   effectively erasing the Amarna period from official chronology. This
 *   constraint demonstrates how interpretive-accretion systems (systems where
 *   cultural meaning is accumulated and sediments as tradition) metabolize
 *   attempted kernel revisions (attempts to alter foundational theological
 *   concepts) not through argument, compromise, or negotiation, but through
 *   erasure, suppression, and authorized denial of legitimacy. The
 *   restoration mechanism coordinates benefits to priestly and military
 *   elites while extracting costs from Atenist adherents and theological
 *   innovation capacity. The theater ratio increases over the interval as
 *   active suppression (early Horemheb destruction campaigns) relaxes into
 *   ritual maintenance (later Ramesside-era commemoration of the erasure
 *   itself becomes the primary function, not continued prevention of Atenist
 *   recurrence). This pattern—initial high extraction enforced through active
 *   suppression, gradually shifting toward performative maintenance of the
 *   erasure itself—is diagnostic of tangled_rope with piton degradation
 *   trajectory.
 *
 * KEY AGENTS:
 *   - Priestly Establishment (Amun temple complex): Institutional beneficiary (institutional/arbitrage) — primary architect of restoration; recovers temple wealth, ritual authority, and interpretive monopoly
 *   - Military Elite (generals, secular officials): Powerful beneficiary (powerful/constrained) — benefits from restored hierarchy that Akhenaten disrupted; coordinates restoration while constrained by need for priestly legitimacy
 *   - Atenist Adherents: Primary victims (powerless/trapped) — forced recantation or exile; theological standing completely eliminated; zero exit options
 *   - Akhenaten's Memory/Legacy: Victim-abstraction (institutional/trapped) — subject to systematic erasure; cannot organize or resist; persists only as negative example
 *   - Horemheb and Military Administration: Enforcer-beneficiary (organized/arbitrage) — executes damnatio memoriae; benefits from associated legitimacy while bearing enforcement costs
 *   - Restoration Coalition (priesthood + military + bureaucracy unified): Organized beneficiary (organized/constrained) — coordinates restoration under sunset logic; extraction presented as temporary emergency compensation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent political choice as metaphysical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amarna_restoration_inner_reassertion, 0.38).
domain_priors:suppression_score(amarna_restoration_inner_reassertion, 0.72).
domain_priors:theater_ratio(amarna_restoration_inner_reassertion, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amarna_restoration_inner_reassertion, extractiveness, 0.38).
narrative_ontology:constraint_metric(amarna_restoration_inner_reassertion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(amarna_restoration_inner_reassertion, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amarna_restoration_inner_reassertion, tangled_rope).
narrative_ontology:human_readable(amarna_restoration_inner_reassertion, "Post-Amarna Restoration as Inner-Container Reassertion").
narrative_ontology:topic_domain(amarna_restoration_inner_reassertion, "ancient_religion/restoration_pattern").

domain_priors:requires_active_enforcement(amarna_restoration_inner_reassertion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amarna_restoration_inner_reassertion, priestly_establishment).
narrative_ontology:constraint_beneficiary(amarna_restoration_inner_reassertion, military_elite).
narrative_ontology:constraint_victim(amarna_restoration_inner_reassertion, akhenaten_memory).
narrative_ontology:constraint_victim(amarna_restoration_inner_reassertion, atenist_adherents).
narrative_ontology:constraint_victim(amarna_restoration_inner_reassertion, theological_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATENIST ADHERENT (SNARE) — Powerless agents who embraced Atenist theology during Akhenaten's reign face maximum suppression post-restoration. Recanting required; continuing faith meant exile, execution, or erasure. No exit option exists except death or total identity surrender. Extraction is complete: the adherent loses theological standing, property, and social position. The constraint operates as pure coercion with zero coordination benefit from the adherent's perspective.
constraint_indexing:constraint_classification(amarna_restoration_inner_reassertion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MILITARY ELITE (TANGLED ROPE) — Powerful agents (generals, officials who survived Amarna) experience the restoration as both coordination and extraction. Coordination benefit: the restoration re-stabilizes the military hierarchy that Akhenaten disrupted by centralizing power in Aten cult. Extraction cost: elites must share restoration spoils with priests and accept theological authority they previously challenged. Exit is constrained by the need to maintain unified opposition to Atenism; defecting risks being branded Atenist sympathizer. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(amarna_restoration_inner_reassertion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIESTLY ESTABLISHMENT (ROPE) — Institutional beneficiary with maximum exit capacity. Priests coordinate the restoration, frame Akhenaten's reign as catastrophic deviation from Ma'at, and recover temple wealth and authority. The constraint functions primarily as coordination: restoring the interpretive frame within which priestly authority is legitimate. Extraction (demanding compensation for temple desecration, requiring royal subordination to priestly protocol) appears from this perspective as justified restoration of proper order, not extraction. Net beneficiary with full agency.
constraint_indexing:constraint_classification(amarna_restoration_inner_reassertion, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DAMNATIO MEMORIAE SYSTEM (PITON) — The systematic erasure mechanism (monument destruction, name removal from succession lists, retro-dating Horemheb's reign) is performatively enforced but structurally inert. Akhenaten's theological innovations persist in doctrinal shadow form (Aten monotheism influences later Judaic concepts). The erasure ritual maintains institutional legitimacy through theatrical destruction, but the intellectual content cannot be fully eliminated. Theater ratio high (65%) because the erasure ritual's primary function is demonstrating priesthood's authority to define history, not actually preventing Atenist influence. The mechanism persists through inertia — once established as the protocol for handling heresy, it becomes standard practice regardless of efficacy.
constraint_indexing:constraint_classification(amarna_restoration_inner_reassertion, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RESTORATION COALITION (SCAFFOLD) — Organized agents across military, priesthood, and bureaucracy coordinate the systematic restoration under a sunset logic: the constraint is framed as temporary emergency measure (restoring proper order) with a built-in termination condition (once Atenism is fully purged and priesthood re-established, enforcement can relax). The coalition's extraction mechanism (temple wealth recovery, elite privilege consolidation) is presented as transitional compensation for Amarna disruption, not permanent structural feature. Enforcement is intensive early (Tutankhamun, Horemheb) and relaxes as Atenist threat recedes (by Seti I, Ramesses era, enforcement becomes ritual maintenance rather than active suppression).
constraint_indexing:constraint_classification(amarna_restoration_inner_reassertion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the restoration appears as inevitable metaphysical law: religious systems naturally resist radical kernel revision; communities automatically reassert anchored interpretive frames when confronted with discontinuity. This perspective sees Akhenaten's failure not as contingent political outcome but as structural necessity — monotheistic revolution cannot succeed against polytheistic establishment without total institutional replacement (which Egypt lacked). However, the structural data contradicts the mountain classification: identifiable beneficiaries (priesthood, military elite) orchestrate the restoration deliberately; the 'inevitability' naturalizes what is actually a political choice to suppress dissent. False summit detection applies here.
constraint_indexing:constraint_classification(amarna_restoration_inner_reassertion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amarna_restoration_inner_reassertion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amarna_restoration_inner_reassertion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amarna_restoration_inner_reassertion, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(amarna_restoration_inner_reassertion, TR),
    TR >= 0.70.

:- end_tests(amarna_restoration_inner_reassertion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The restoration coordinates genuine priestly and military interests (recovery of authority structures genuinely disrupted by Akhenaten) while extracting significant costs from adherents and innovation capacity. The value reflects that some coordination benefit is real (stabilizing hierarchy), but asymmetric extraction is substantial (temple wealth recovery, labor conscription, theological monopoly restoration). The trajectory shows rapid rise from 0.15 (immediate post-Amarna chaos) to 0.38-0.42 (consolidation phase) as enforcement mechanisms solidify, then plateau as extraction becomes routine. Suppression (0.72): High. Active barriers to Atenist recurrence include monument destruction, name erasure, theological prohibition, and capital punishment for heresy. But suppression is not absolute—Akhenaten's theology influenced later monotheistic traditions—indicating that suppression mechanisms have structural limits despite high-intensity enforcement. Theater ratio (0.65): Moderate-high. The damnatio memoriae ritual (monument destruction, name removal, retro-dating) functions partly as genuine enforcement (preventing Atenist organization) but increasingly as performance (the ritual of erasure itself becomes the demonstration of authority, more important than the prevented harm). As the Atenist threat genuinely recedes post-Horemheb, the theater ratio should decline slightly, reflecting transition from enforcement to maintenance—this is captured in the measurement trajectory (0.45 → 0.65 → 0.58).
 *
 * PERSPECTIVAL GAP:
 *   The restoration demonstrates the full spectrum of constraint perception across positions. The priestly establishment sees legitimate order restoration (rope). The military elite sees mixed coordination-extraction (tangled_rope). The Atenist adherent sees pure suppression (snare). The erasure system sees performative maintenance (piton). The restoration coalition sees transitional emergency (scaffold). The civilizational observer sees metaphysical inevitability (mountain). No single type captures the constraint from all perspectives because the constraint is fundamentally asymmetric—it redistributes authority and resources in favor of priesthood/military, against theological innovation and Atenist adherents. The perspectival gap is not a measurement error but a diagnostic feature: the gap itself reveals the asymmetric structure. The mountain perspective is particularly instructive as a false summit—it naturalizes what is actually a political choice by framing the restoration as inevitable reaction to heresy. But other societies handled theological crises differently (Buddhism's gradual absorption into Hinduism, Christianity's incorporation of mystery religions, Zoroastrianism's coexistence with polytheistic elements). Egypt's restoration was one contingent path among multiple possibilities. The gap between 'inevitable' (mountain) and 'political choice by interested agents' (tangled_rope/snare) is the diagnostic evidence for false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives directionality (d) from each agent's structural position: beneficiary status, victim status, and exit options. Priestly establishment: beneficiary + arbitrage exit → d ≈ 0.05-0.15 → f(d) ≈ -0.10 → negative/low effective extraction (experiences constraint as coordination serving their interests). Military elite: beneficiary + constrained exit → d ≈ 0.35-0.45 → f(d) ≈ 0.35-0.50 → moderate effective extraction (experiences constraint as mixed coordination and compensation requirement). Atenist adherent: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum effective extraction (experiences constraint as pure suppression). Horemheb/military administration: beneficiary + organized status → d ≈ 0.20-0.30 → f(d) ≈ 0.05-0.15 → low effective extraction (enforcer-beneficiary with substantial agency). Restoration coalition: beneficiary with sunset clause + organized status → d ≈ 0.25-0.35 → f(d) ≈ 0.20-0.35 → moderate effective extraction (transitory extraction justified as emergency restoration). The scope modifier σ(S) applies uniformly to all perspectives (constraint operates at national scope, σ(national) = 1.0), so the chi formula χ = ε × f(d) × 1.0 produces perspectival divergence purely from agent position and exit options, not from scope effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how an interpretive-accretion system (priestly establishment with theological monopoly) metabolizes attempted kernel revision (Akhenaten's monotheism) through enforced reversion rather than negotiated synthesis. The system cannot accommodate radical theological revision because the priestly authority structure IS the interpretation authority—to accept the revision would be to deny the priesthood's role as keeper of Ma'at. Instead, the system executes damnatio memoriae: it denies that the revision ever had legitimate standing, erases it from official history, and restores the pre-revision state while extracting compensation for the disruption. This is not rope (pure coordination) because the cost-distribution is asymmetric and enforced through suppression. This is not snare (pure extraction) because there is genuine coordination benefit for military elite and priestly establishment—their authority structure was genuinely disrupted and genuinely needs restabilization. This is tangled_rope: coordination function (restoring proper hierarchy) bundled with asymmetric extraction (costs borne by Atenists, compensation demanded from elite, priestly authority expanded). The mandatrophy resolves by recognizing that the system's fundamental incompatibility with revision is not a natural law but a structural feature of how priestly authority grounds itself in interpretive monopoly. Alternative institutional structures (e.g., councils with theological pluralism, scribal schools with competing interpretation traditions) might accommodate revision differently. The restoration's 'inevitability' is contingent on Egypt's specific institutional configuration at that historical moment, not on metaphysical necessity. The false summit perspective (mountain classification) reveals what the system benefits from hiding: that the restoration is a political choice by agents with structural interest in suppression, framed as metaphysical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    akhenaten_theological_continuity,
    'Did Akhenaten''s monotheistic innovation truly disappear, or did it persist in disguised form within later Egyptian theology and influence subsequent Abrahamic traditions?',
    'Textual analysis of post-Amarna theological writings; comparison with Judaic monotheistic formulations; investigation of Akhenaten-Moses syncretism theories',
    'If continuity confirmed: restoration''s suppression was partial; Atenism persists underground. If no continuity: restoration fully metabolized the revision attempt, validating piton analysis. Either outcome supports the constraint structure — the restoration mechanism suppresses explicitly while the constraint itself (interpretive-accretion system) has inherent capacity to absorb innovations covertly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(akhenaten_theological_continuity, empirical, 'Whether Atenist theology persisted covertly after restoration').

omega_variable(
    horemheb_retro_dating_mechanism,
    'Was Horemheb''s retro-dating (dating his reign from Amenhotep III, skipping the Amarna period) a practical administrative choice or a deliberate metaphysical erasure strategy with theological implications?',
    'Analysis of administrative records and dating conventions before/after restoration; examination of whether retro-dating was standard practice or unprecedented for Amarna period',
    'If practical administrative choice: restoration was about institutional recovery, not metaphysical erasure. If deliberate erasure strategy: restoration demonstrates explicit use of temporal manipulation to deny legitimacy. Classification would shift Scaffold perspective toward Snare if erasure was coercive rather than transitional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(horemheb_retro_dating_mechanism, conceptual, 'Whether retro-dating was administrative or metaphysical erasure').

omega_variable(
    priestly_extraction_versus_legitimate_restoration,
    'How much of the priestly compensation (temple wealth recovery, labor conscription for reconstruction) represents legitimate restoration of seized property versus extractive exploitation of post-Amarna chaos?',
    'Comparative accounting of temple wealth before Akhenaten, during Amarna, and post-restoration; analysis of labor demands and economic burden on non-priestly population during reconstruction phase',
    'If legitimate restoration: Tangled Rope classification confirmed at moderate extraction. If significant exploitation: extraction rises toward Snare territory (ε > 0.46) and suppression rises toward 0.80+. Directionality for military elite and restoration coalition shifts toward victim status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_extraction_versus_legitimate_restoration, empirical, 'Proportion of priestly gain attributable to restoration versus extraction').

omega_variable(
    inner_container_stability_mechanism,
    'Is the restoration''s success (preventing Atenism''s recurrence) primarily due to active enforcement mechanisms (erasure, suppression) or due to the underlying stability of the priestly interpretive frame itself?',
    'Historical analysis of Atenist recurrence attempts post-restoration; comparison with other failed religious revisions in Mediterranean history; assessment of enforcement necessity versus cultural reversion',
    'If active enforcement dominant: constraint is externally maintained Snare/Tangled Rope, fragile without continuous suppression. If interpretive frame dominance: constraint is self-reinforcing Rope, stable without enforcement (piton analysis wrong). Outcome determines whether mountain perspective has any validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inner_container_stability_mechanism, empirical, 'Whether restoration stability derives from enforcement or interpretive reversion').

omega_variable(
    false_summit_naturalization,
    'Does the constraint genuinely represent an inevitable feature of how religious systems handle revision attempts, or does it represent a contingent political outcome that benefits specific agents and would be experienced differently in a counterfactual scenario (e.g., if Akhenaten had consolidated power longer, or if priests had weaker institutional leverage)?',
    'Comparative historical analysis of successful theological innovations (Zoroastrianism, Buddhism, Christianity) and failed ones; assessment of causal necessity versus path contingency; examination of counterfactual robustness',
    'If naturalization confirmed as false: mountain perspective is engineered consent, not natural law. Constraint is Snare/Tangled Rope with high theater (piton elements) masking contingent power dynamics. If some naturalization valid: constraint has mixed natural law and political extraction components, requiring refined subtype analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether restoration represents natural law or contingent political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amarna_restoration_inner_reassertion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amarna_tr_t0, amarna_restoration_inner_reassertion, theater_ratio, 0, 0.45).
narrative_ontology:measurement(amarna_tr_t5, amarna_restoration_inner_reassertion, theater_ratio, 5, 0.65).
narrative_ontology:measurement(amarna_tr_t10, amarna_restoration_inner_reassertion, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(amarna_be_t0, amarna_restoration_inner_reassertion, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(amarna_be_t5, amarna_restoration_inner_reassertion, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(amarna_be_t10, amarna_restoration_inner_reassertion, base_extractiveness, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amarna_restoration_inner_reassertion, identity_coordination).
narrative_ontology:affects_constraint(amarna_restoration_inner_reassertion, atenist_theological_innovation).
narrative_ontology:affects_constraint(amarna_restoration_inner_reassertion, damnatio_memoriae_protocol).
narrative_ontology:affects_constraint(amarna_restoration_inner_reassertion, priestly_authority_consolidation).

% DUAL FORMULATION NOTE:
% The Amarna restoration decomposes into three structurally distinct constraints: (1) Atenist theological innovation (ε≈0.25, mountain for Atenists, rope for innovators—claim to establish monotheism), (2) Damnatio memoriae protocol (ε≈0.35, piton—enforcement ritual whose primary function is authority demonstration rather than prevention of genuine threat), (3) Priestly authority consolidation (ε≈0.38, tangled_rope—genuine coordination benefit for priestly establishment bundled with extraction from subordinate agents). This story covers the integrated system response; the constraint family links show how the restoration works as a unified mechanism combining theological negation, historical erasure, and institutional power recovery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amarna_restoration_inner_reassertion, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
