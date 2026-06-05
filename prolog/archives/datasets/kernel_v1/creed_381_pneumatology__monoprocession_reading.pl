% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__monoprocession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__monoprocession_reading, []).

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
 *   constraint_id: creed_381_pneumatology__monoprocession_reading
 *   human_readable: Council of Constantinople 381: Spirit Proceeds from Father Alone (Monoprocession Reading)
 *   domain: historical_theology/ecclesiastical_authority/commitment_systems
 *
 * SUMMARY:
 *   The Council of Constantinople 381 affirmed that the Holy Spirit proceeds
 *   from the Father alone (monoprocession), and declared that any unilateral
 *   amendment to the shared creed violates the binding consensus and
 *   constitutes breach of the ecumenical compact. This constraint operates as
 *   a structural wall preventing any single ecclesiastical see — especially
 *   the Roman see — from legislating doctrine for the whole Church without
 *   explicit consent of the Eastern autocephalous churches. The
 *   monoprocession reading treats the constraint as genuinely doctrinal (the
 *   Spirit's procession is a real claim about divine causation) AND
 *   structurally protective (the inviolability rule defends Eastern
 *   ecclesiastical autonomy). The constraint exhibits the full trajectory
 *   from functional tangled rope (at Council 381, where coordination and
 *   enforcement both operate) to piton (by the Reformation, where the creed's
 *   inviolability is cited ceremonially while unilateral amendments
 *   accumulate in practice). The Western adoption of Filioque (Spirit from
 *   Father and Son) without formal Eastern consent represents the first major
 *   breach of the 381 constraint, though the breach was gradual — Filioque
 *   entered Western liturgical practice silently over centuries before being
 *   formally acknowledged as doctrine. By the High Medieval period, the
 *   constraint's enforcement mechanism had essentially collapsed, yet the
 *   invocation of 'inviolability' persisted as legitimacy theater. The
 *   Eastern sees cite 381's monoprocession as the authoritative reading; the
 *   Western sees eventually reinterpret it through Filioque lens or declare
 *   the amendment justified as theological development. This reading — the
 *   monoprocession reading — is one of three structurally distinct readings
 *   of the same nominal kernel (the 381 creed). Its competitors are the
 *   Filioque reading (Western doctrinal innovation is legitimate) and the
 *   ecumenical-reunion reading (multiple readings can coexist, schism need
 *   not be permanent). This constraint story instantiates the monoprocession
 *   reading only.
 *
 * KEY AGENTS:
 *   - Eastern Autocephalous Churches (Constantinople, Alexandria, Antioch, Jerusalem): Primary beneficiary (institutional/arbitrage) — constraint protects their ecclesiastical autonomy and gives them veto power over Western doctrinal innovation
 *   - Roman See / Western Latin Church: Primary victim AND secondary beneficiary — experiences suppression of innovation capacity (snare perspective), but also benefits from coordination function and maintenance of doctrinal unity at local level (tangled rope perspective)
 *   - Dissenting Western Theologians: Victim (powerless/trapped) — constrained by threat of schism and excommunication from pursuing innovations perceived as legitimate theological development
 *   - Council of Constantinople 381 (Ecumenical Assembly): Organized agent at t0 — genuinely deliberative, functional enforcement mechanism; experiences low theater
 *   - Post-Medieval Ecclesiastical Authority: Institutional actor post-t500 — maintains constraint as ceremonial citation while enforcement decays; high theater, degraded function
 *   - Doctrinal Stability Itself: Abstract victim (powerless/trapped) — the creed's inviolability claim becomes theatrical as unilateral amendments accumulate; field loses coherent identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, 0.58).
domain_priors:suppression_score(creed_381_pneumatology__monoprocession_reading, 0.62).
domain_priors:theater_ratio(creed_381_pneumatology__monoprocession_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__monoprocession_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__monoprocession_reading, "Council of Constantinople 381: Spirit Proceeds from Father Alone (Monoprocession Reading)").
narrative_ontology:topic_domain(creed_381_pneumatology__monoprocession_reading, "historical_theology/ecclesiastical_authority/commitment_systems").

domain_priors:requires_active_enforcement(creed_381_pneumatology__monoprocession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__monoprocession_reading, '93c916a3-f0b1-46e5-96bd-58cfee0e33eb').
narrative_ontology:cs_kernel_codification('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', fixed_text).
narrative_ontology:cs_authority_grounding('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', lineage).
narrative_ontology:cs_interpretation_layer_present('93c916a3-f0b1-46e5-96bd-58cfee0e33eb').
narrative_ontology:cs_reading_relation('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', creed_381_pneumatology__filioque_reading, forecloses).
narrative_ontology:cs_reading_relation('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', creed_381_pneumatology__ecumenical_reunion_reading, coexists_with).
narrative_ontology:cs_axiom('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', foundational, spirit_procession_monoprocession_doctrine).
narrative_ontology:cs_axiom_status(spirit_procession_monoprocession_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', spirit_procession_monoprocession_doctrine, deontological).
narrative_ontology:cs_axiom('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', foundational, creed_amendment_requires_ecumenical_consent).
narrative_ontology:cs_axiom_status(creed_amendment_requires_ecumenical_consent, holdable).
narrative_ontology:cs_axiom_grounding('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', creed_amendment_requires_ecumenical_consent, conventional).
narrative_ontology:cs_reference_frame('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', council_381_ecumenical_polity).
narrative_ontology:cs_drift_state('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', late_medieval_post_filioque_adoption, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('93c916a3-f0b1-46e5-96bd-58cfee0e33eb', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, doctrinal_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING WESTERN THEOLOGIAN (SNARE) — Trapped within a confessional tradition that will eventually be declared heretical by Eastern sees; innovation is suppressed by threat of schism and excommunication. No exit without abandoning Occidental ecclesiastical legitimacy entirely. Maximum experienced extraction — innovation is coercively foreclosed.
constraint_indexing:constraint_classification(creed_381_pneumatology__monoprocession_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ROMAN SEE'S AUTHORITY STRUCTURE (TANGLED ROPE) — Experiences genuine coordination benefit (defining orthodoxy for its constituency), but also bears cost of constraint: cannot unilaterally amend the shared creed without generating schism and reducing its authority over the Eastern sees. Constrained by the need for ecumenical consent, but benefits from coordination function. Mixed extraction and coordination.
constraint_indexing:constraint_classification(creed_381_pneumatology__monoprocession_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: EASTERN AUTOCEPHALOUS CHURCHES (ROPE) — Primary beneficiaries. The constraint protects their structural autonomy: any Western unilateral amendment is automatically breach, triggering their exit option (ecumenical rupture) and confirming their jurisdictional independence. They experience the constraint as pure coordination — it solves the collective action problem of maintaining doctrinal unity without centralizing authority in Rome.
constraint_indexing:constraint_classification(creed_381_pneumatology__monoprocession_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ECUMENICAL COALITION AT COUNCIL 381 (TANGLED ROPE) — Organized agents (bishops assembled at Constantinople) experience coordination function (defining shared orthodoxy) alongside enforcement cost (maintaining consensus across diverse sees). Mobile in principle but constrained in practice by the need to preserve unanimity. Theater is low here — the council's decision-making process is genuine deliberation, not ritual.
constraint_indexing:constraint_classification(creed_381_pneumatology__monoprocession_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 5: DECENTRALIZED AUTHORITY NORMS (PITON) — By the Reformation era and after, the constraint's primary function (preventing unilateral amendment) has degraded into ritual citation. The creed is treated as inviolable, but enforcement is theatrical — ecumenical consent is invoked as a formal requirement while unilateral amendments accumulate (Filioque silently adopted in Western liturgy for centuries before formal recognition). The constraint persists through inertia and legitimacy citation, not through actual functional enforcement.
constraint_indexing:constraint_classification(creed_381_pneumatology__monoprocession_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the constraint appears to be a logical necessity: any shared creed binding multiple autonomous sees MUST have amendment protection, or doctrinal unity dissolves. The creed-as-binding-document produces this constraint necessarily. However, this naturalizes a contingent institutional choice: the creed could have been treated as locally interpretable (e.g., Eastern Orthodoxy's approach to received texts), or as a lower-stakes coordination device with looser amendment rules. The analytical observer risks false summit by treating a particular institutional arrangement as inevitable.
constraint_indexing:constraint_classification(creed_381_pneumatology__monoprocession_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__monoprocession_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creed_381_pneumatology__monoprocession_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creed_381_pneumatology__monoprocession_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creed_381_pneumatology__monoprocession_reading, TR),
    TR >= 0.70.

:- end_tests(creed_381_pneumatology__monoprocession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.58 at t=1400): Moderate-high. At the Council (t=0), extractiveness is low (0.35) because genuine deliberation and consensus-building produce real coordination. By the High Medieval period (t=1000), extractiveness climbs to 0.62 as the Western see increasingly treats the constraint as an obstacle to legitimate theological development, suppressing innovation in the periphery (dissenting theologians) while the center (Rome) slowly accumulates unilateral modifications. The creep reflects increasing asymmetry: Western sees view Eastern consent requirements as parochial; Eastern sees view Western innovations as breaches. SUPPRESSION (0.62): Moderate-high. The mechanism combines structural suppression (threat of schism, excommunication) and internalized suppression (the constraint becomes part of ecclesiastical legitimacy, making deviation feel like heresy rather than lawful disagreement). THEATER (0.48 → 0.68): Rises sharply post-t500. At the Council, theater is low because decision-making is functional. Post-Medieval, invocation of '381's inviolability' becomes ceremonial — the constraint is cited to block innovation, but enforcement has collapsed. By t=1400, theater (0.68) indicates piton-level theatrical persistence. The measurements show functional decay with ceremonial maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The Eastern sees (Rope perspective) experience the constraint as protecting their structural autonomy — it prevents Western centralization of authority. The Roman see (Tangled Rope perspective) experiences genuine coordination benefit (ability to define orthodoxy for its constituency) but also suppression of its own innovation capacity relative to what it could achieve unilaterally. Dissenting Western theologians (Snare perspective) experience pure extraction — they are trapped by threat of schism and heresy condemnation, unable to pursue what they perceive as legitimate theological development. The ecumenical council at t0 (Tangled Rope/organized) experiences real deliberation and mixed benefit-cost. Post-Medieval ecclesiastical authority (Piton) experiences its own constraint as degraded ritual — the inviolability is maintained ceremonially while enforcement capacity has atrophied. The analytical observer (Mountain) risks naturalizing the constraint as a logical necessity (any creed binding multiple autonomous sees must have amendment protection), but this naturalizes a particular institutional choice (the creed as binding document with amendment veto) rather than alternatives like locally interpretable traditions or looser coordination rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for each perspective flows from beneficiary/victim status and exit options. Eastern autocephalous churches: beneficiaries with arbitrage-level exit (they can rupture communion if breached) → low d → negative effective extraction → Rope. Roman see: institutional actor with constrained exit (breaching the constraint triggers schism) + mixed beneficiary/victim status → moderate d → mixed extraction → Tangled Rope. Dissenting Western theologian: victim with trapped exit (cannot innovate without schism) → high d → high effective extraction → Snare. Council-era bishops (organized/mobile): organized actors with genuine agency in deliberation → moderate d → moderate extraction → Tangled Rope. Post-Medieval authority (institutional/constrained): maintaining ceremonial constraint with collapsed enforcement → moderate d but degraded function → Piton (theater gate, not chi gate). Analytical observer: canonical d (0.73) for analytical power, but mountain classification is perspectival — captures the risk of false summit (naturalization of contingent institutional choice).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the same nominal creed-amendment rule can be tangled_rope (coordination function present: defines shared orthodoxy, prevents unilateral centralization) AND snare (for dissenting Western theologian: innovation is suppressed without coordination benefit to that agent). The resolution is not a single classification but a perspectival presheaf: different agents at different power/exit positions experience the constraint differently. For Eastern sees, it is genuine coordination (Rope). For Rome, it is mixed coordination and suppression (Tangled Rope). For innovators, it is pure suppression (Snare). For the 381 Council, it is deliberative coordination (Tangled Rope). For post-Medieval authorities, it is theatrical inertia (Piton). The analytical observer must avoid false summit by recognizing that 'any binding creed needs amendment protection' is a particular institutional choice, not a logical necessity. Alternative arrangements (local interpretability, loose coordination, non-binding creeds) exist in other traditions and are structurally viable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monoprocession_core_premise,
    'Is the monoprocession doctrine (Spirit from Father alone) a genuine doctrinal claim about divine causation, or a jurisdictional claim about ecclesiastical authority disguised as pneumatology?',
    'Theological analysis of the Council of 381 proceedings and Cappadocian sources; distinction between pneumatological necessity claims vs. authority preservation claims in the text; examination of whether monoprocession is treated as falsifiable doctrine or as constitutive of the creed''s inviolability',
    'If doctrinal: the constraint is about maintaining a specific theological truth and its defense is structurally legitimate. If jurisdictional: the constraint is about protecting Eastern ecclesiastical autonomy and the extraction dimension becomes primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monoprocession_core_premise, conceptual, 'Whether monoprocession is doctrine or jurisdictional claim').

omega_variable(
    filioque_logical_coexistence,
    'Can the Filioque (Spirit proceeds from Father AND Son) logically coexist with monoprocession within a single coherent framework, or does monoprocession FORECLOSE the Filioque?',
    'Formal theological analysis: can a framework maintain ''Spirit from Father alone'' and ''Spirit from Father and Son'' as both true without contradiction? Historical examination of whether medieval theologians attempted reconciliation or treated the claims as mutually exclusive.',
    'If coexistence is possible: reading_relations should be ''coexists_with''. If monoprocession logically forecloses Filioque: reading_relations should be ''forecloses''. If one merely influences the other''s legitimacy conditions: reading_relations should be ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(filioque_logical_coexistence, conceptual, 'Logical relationship between monoprocession and Filioque doctrines').

omega_variable(
    ecumenical_consent_enforcement_decay,
    'At what point did the constraint''s enforcement mechanism (ecumenical consent as brake on amendment) actually cease to function, and what caused the transition from functional constraint to ceremonial citation?',
    'Historical documentation of: (1) when Filioque was adopted unilaterally in Western liturgy without formal ecumenical consent; (2) when Eastern sees formally condemned the amendment but Western sees proceeded anyway; (3) dating of the functional collapse of the co-decision procedure.',
    'If decay began c. 800-900 CE: the constraint shifted from tangled_rope (coordination + enforcement) to piton (theatrical persistence) for 1100+ years. If decay was gradual (incremental amendments accumulating 800-1500): theater_ratio should increase across measurements, showing progressive loss of function. Affects piton classification confidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecumenical_consent_enforcement_decay, empirical, 'Timeline of enforcement decay from functional to theatrical').

omega_variable(
    filioque_eastern_innovation_asymmetry,
    'Did Eastern sees also make unilateral doctrinal modifications that would breach the 381 constraint if symmetrically evaluated, or was innovation genuinely unilateral from the West?',
    'Historical comparison: list Eastern doctrinal innovations post-381 (e.g., Theotokos development, icon theology) and Western ones (Filioque, papal infallibility, immaculate conception); assess whether breach claim is asymmetrically applied or whether Eastern innovation actually did occur without Western consent.',
    'If Eastern innovation also occurred: the constraint operates asymmetrically as power structure (Western innovation = breach, Eastern innovation = development). Strengthens interpretation of constraint as jurisdictional (protecting Eastern autonomy) rather than doctrinal. If only Western innovation: constraint is applied symmetrically and functions as genuine doctrinal stabilizer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filioque_eastern_innovation_asymmetry, empirical, 'Whether constraint is applied symmetrically or asymmetrically across sees').

omega_variable(
    kernel_reading_distinguisher,
    'COMMITTER FRAME: This constraint is one reading of the contested kernel (creed_381_pneumatology). What is the logical or structural difference between the monoprocession reading, the Filioque reading, and the ecumenical-reunion reading, and are these readings held as live positions by actual parties today?',
    'Identify three distinct theological positions: (1) monoprocession reading = Spirit from Father alone, unilateral amendment breaches creed, Eastern sees have veto; (2) Filioque reading = Spirit from Father and Son, Western development is legitimate theological advancement, creed should be updated or reinterpreted; (3) ecumenical-reunion reading = creed is culturally malleable, multiple readings can coexist, schism need not be permanent. Trace which modern communities hold which reading.',
    'Confirms that three structurally distinct constraints exist for the same nominal referent (the 381 creed). Validates decomposition into separate stories. Clarifies which reading_relations hold between siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinguisher, conceptual, 'Structural distinguishability of three readings of 381 pneumatology kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__monoprocession_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(creed381_mono_theater_t0_council_381, creed_381_pneumatology__monoprocession_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(creed381_mono_theater_t500_early_medieval, creed_381_pneumatology__monoprocession_reading, theater_ratio, 500, 0.35).
narrative_ontology:measurement(creed381_mono_theater_t1000_high_medieval, creed_381_pneumatology__monoprocession_reading, theater_ratio, 1000, 0.52).
narrative_ontology:measurement(creed381_mono_theater_t1400_late_medieval, creed_381_pneumatology__monoprocession_reading, theater_ratio, 1400, 0.68).

% Extraction over time
narrative_ontology:measurement(creed381_mono_extractiveness_t0_council_381, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(creed381_mono_extractiveness_t500_early_medieval, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 500, 0.48).
narrative_ontology:measurement(creed381_mono_extractiveness_t1000_high_medieval, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 1000, 0.62).
narrative_ontology:measurement(creed381_mono_extractiveness_t1400_late_medieval, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 1400, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(creed381_mono_suppression_t0_council_381, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(creed381_mono_suppression_t500_early_medieval, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 500, 0.55).
narrative_ontology:measurement(creed381_mono_suppression_t1000_high_medieval, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(creed381_mono_suppression_t1400_late_medieval, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 1400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__monoprocession_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% The 381 creed is the kernel for three structurally distinct constraints: monoprocession_reading (ε=0.58, functional tangled_rope at t0 declining to piton at t1400), filioque_reading (ε≈0.52, Western innovation asymmetry as systemic extraction), and ecumenical_reunion_reading (ε≈0.48, soft coordination with plastic boundaries). Each reading has different beneficiary/victim structures, different enforcement mechanisms, and different theater trajectories. They are not observable variants of a single constraint — they are three constraints unified by shared kernel text but diverging in how they interpret the kernel's binding force. The ε-invariance principle applies: if measuring the constraint via monoprocession doctrine gives ε=0.58 but measuring via Filioque legitimacy gives ε≈0.52, two distinct constraints are revealed, not two measurements of one. Network decomposition preserves the kernel identity while recognizing structural differentiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creed_381_pneumatology__monoprocession_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
