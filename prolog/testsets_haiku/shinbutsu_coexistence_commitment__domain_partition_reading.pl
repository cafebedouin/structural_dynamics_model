% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Coexistence: Domain Partition (Kami/Buddhist Separation of Governance)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   This constraint story models ONE reading of a contested kernel: the
 *   Shinbutsu Coexistence (shinbutsu shugo) arrangement that governed
 *   Japanese religious practice from the Heian period onward. The DOMAIN
 *   PARTITION READING interprets the arrangement as a stable, functionally
 *   justified separation of governance domains: Shinto (life, purity,
 *   agricultural prosperity) and Buddhism (death, salvation, afterlife)
 *   operate as non-overlapping systems that resolve theological incoherence
 *   through boundary maintenance rather than doctrinal synthesis. This
 *   reading vindicates functional pluralism and treats popular practice as
 *   the authoritative ground. It is corroborated by traditional Shinto and
 *   Buddhist sources, institutional records of centuries of coexistence, and
 *   the observed mutual non-interference of the two systems. Sibling readings
 *   (syncretic_fusion_reading via honji suijaku theory,
 *   incoherent_bundle_reading via Meiji critique) claim the partition
 *   conceals unresolved theological conflict or deliberate institutional
 *   obscuration. The engine computes this reading's classification from the
 *   structural data; the reading's claim of stable rope (coordination without
 *   coercive breakdown) is independent of the authored metrics.
 *
 * KEY AGENTS:
 *   - Shinto priesthood: Maintains kami shrines and life-domain rituals; enforces the partition by refusing death-ritual authority
 *   - Buddhist monastic establishment: Administers funerary and afterlife theology; enforces the partition by not claiming agricultural authority
 *   - Peasant communities: Primary participants in both systems; benefit from not having to choose or synthesize doctrines
 *   - Court aristocracy: Patronizes both systems for political legitimacy without requiring theological unification
 *   - Scholastic theologians (honji suijaku): Excluded voices arguing for ontological unification; treated as incoherent under domain partition
 *   - Meiji reformers: External observers (late 19th century) who reread the partition as unstable maintenance of Buddhist dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.38).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.42).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Coexistence: Domain Partition (Kami/Buddhist Separation of Governance)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, 'ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af').
narrative_ontology:cs_kernel_codification('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', implicit).
narrative_ontology:cs_authority_grounding('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', practice).
narrative_ontology:cs_interpretation_layer_present('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af').
narrative_ontology:cs_reading_relation('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', shinbutsu_coexistence_commitment__syncretic_fusion_reading, influences).
narrative_ontology:cs_reading_relation('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', foundational, existential_domain_separation_justified).
narrative_ontology:cs_axiom_status(existential_domain_separation_justified, holdable).
narrative_ontology:cs_axiom_grounding('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', existential_domain_separation_justified, conventional).
narrative_ontology:cs_axiom('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', foundational, functional_coexistence_over_theological_consistency).
narrative_ontology:cs_axiom_status(functional_coexistence_over_theological_consistency, holdable).
narrative_ontology:cs_axiom_grounding('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', functional_coexistence_over_theological_consistency, instrumental).
narrative_ontology:cs_reference_frame('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', dual_institutional_stability).
narrative_ontology:cs_drift_state('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', late_edo_period_1750_1868, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ebf6d862-2e36-4c1a-b1ec-1b9c25bee7af', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_monastic_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, peasant_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 at interval end) is moderate because the constraint does extract institutional patronage and ritual participation, but the extraction is not predatory — both institutions provide genuine functional services within their domains. The extraction increases over time (0.18→0.38) as Buddhist funerary practices become economically crucial and as Shinto priesthood develops organizational capacity. Suppression (0.42) is moderate because the constraint's persistence requires enforcing the boundary (suppressing unification movements, blocking cross-domain claims), but the suppression is not violent — it operates through institutional refusal and interpretive authority rather than coercion. Theater (0.51) rises sharply early (0.25→0.44 by Heian-Kamakura), suggesting that maintaining the partition increasingly requires performative assertion: priests must actively claim they 'only handle life' or 'only handle death,' and this performance becomes more elaborate as theoretical challenges (honji suijaku) mount. By late Edo (1750-1868), theater plateaus at 0.51, indicating sustained performative maintenance without further escalation. Accessibility collapse (0.61) is moderate-high because alternatives to the partition do theoretically exist (honji suijaku unification, Buddhist dominance, Shinto-only revival), but they are institutionally and socially suppressed by centuries of precedent and mutual institutional investment. Resistance (0.48) is moderate, indicating that some voices (scholastic theologians, later reformers) actively resist the partition's framing as natural or final, but resistance never achieves dominance within the traditional system — it requires external modernizing pressure (Meiji) to break the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The Shinto and Buddhist establishments experience this constraint as a beneficial coordination (rope): each gains institutional space, revenue, and legitimacy without theological compromise. Peasants experience it as a beneficial dual-access arrangement (some rope, some beneficiary). Court aristocracy experiences it as instrumental (useful for legitimacy, but movable if modernization demands). Scholastic theologians experience it as irrational suppression of truth (honji suijaku would resolve the incoherence). Meiji reformers experience it as disguised Buddhist dominance requiring correction. The domain-partition reading assigns authority to peasant and institutional experience (functional coexistence works; theology is secondary), which systematically marginalizes the scholastic and reform perspectives. The engine computes per-seat directionality from the structural data: Shinto and Buddhist establishments sit near d=0.2-0.3 (net beneficiaries), peasants near d=0.5 (symmetric access + modest suppression of doctrinal questioning), theologians and reformers as powerless-excluded (high d, but excluded from steering the constraint). This divergence is structural, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priesthood: Agenda-setter, organized power, constrained exit (tied to shrine institutions). Beneficiary (gains exclusive life-domain authority without theological competition). Directionality: low, near 0.2 (full beneficiary). Buddhist monastic establishment: Agenda-setter, organized power, constrained exit. Beneficiary (gains exclusive afterlife/salvation authority, economically crucial funerary revenue). Directionality: low-moderate, near 0.25 (full beneficiary, but must actively maintain the partition boundary). Peasant communities: Powerless, constrained exit (no institutional independence), local scope. Role: beneficiary and implicit payer (participate in both systems, cannot exit without social cost, must accept non-unification of theology). Directionality: moderate-high, near 0.5-0.55 (symmetric: genuine benefit from dual access, modest cost from suppression of doctrinal questioning). Court aristocracy: Powerful, mobile exit (can shift patronage or change state policy). Beneficiary (uses both systems for legitimacy without synthesis cost). Directionality: low, near 0.15 (strong beneficiary, highest exit options buffer the cost). Scholastic theologians: Moderate power, mobile exit (can publish, travel, teach). Excluded (their unification theory is not treated as authoritative). Victim (their intellectual project is suppressed by treating domain partition as settled). Directionality: high, near 0.75 (target of boundary enforcement, excluded from governance). Meiji reformers: Institutional power, mobile exit (state capacity to rewrite institutional rules). Observer and external challenger (not part of the traditional system). Directionality: analytical, 0.5 (they observe the constraint from outside and eventually break it).
 *
 * MANDATROPHY ANALYSIS:
 *   The domain partition reading must resist two distinct mandatrophy vectors: (1) functional obsolescence (if Shinto and Buddhism separately solve their domains perfectly, what mandate remains?), and (2) theoretical instability (if honji suijaku theory is correct, the partition is false and should be overcome). The rope classification depends on treating functional separation as legitimate even when theological unification is theoretically possible. The reading's defense is that coordination function is not reducible to theological consistency — peasant communities genuinely benefit from not having to choose or synthesize, and institutional coexistence is self-sustaining over centuries. The reading's vulnerability is precisely that the mandate is functional (prevent theological collapse of the dual system) and the function is preserved by perpetually refusing to resolve the underlying incoherence. Once a state actor (Meiji) decides that Shinto recovery requires separation, the partition constraint collapses — it had no mandate independent of institutional mutual interest. The domain-partition reading classifies the constraint as a stable rope precisely because it treats functional coexistence and institutional beneficiaries as sufficient; the incoherent-bundle reading (sibling) reclassifies the same facts as unstable false summit (natural-law presentation of what is actually institutional maintenance). The engine's per-seat computation exposes this: from peasant and institutional seats, it appears as rope; from scholastic and reform seats, it appears as enforced false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_stability_naturalness,
    'Is the domain partition a natural, stable solution to theological irreconcilability, or an artificial maintenance of incoherence that requires continuous institutional effort to prevent collapse?',
    'Comparative analysis: (1) Historical records of explicit boundary-maintenance actions and theological disputes requiring suppression. (2) Natural-experiment comparison with cultures where similar theological tensions arose and were resolved (through synthesis or suppression of one system). (3) Post-Meiji analysis: does Shinto recovery after forced separation show the partition was artificial, or does it show Meiji intervention was artificial?',
    'If partition is natural/stable (first reading holds), the constraint classifies as rope with high sustainability — boundary maintenance is voluntary institutional interest. If partition is artificial (second reading holds), the constraint downclassifies toward piton (maintained by institutional inertia despite no deep functional stability) or snare (Buddhist power masquerading as coexistence). The measurement of theater_ratio (0.25→0.51) suggests increasing performative work, which slightly favors the artificial-maintenance interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_naturalness, empirical, 'Whether domain partition is a stable solution or artificial institutional maintenance').

omega_variable(
    honji_suijaku_suppression_mechanism,
    'Why did honji suijaku (ontological unification theory) remain a marginal scholastic position despite being intellectually sophisticated, rather than becoming the governing framework for Shinto-Buddhist relations?',
    'Historical analysis of institutional gatekeeping: (1) Did Shinto and Buddhist hierarchies actively reject honji suijaku because it threatened their institutional interests? (2) Was it suppressed by state authority? (3) Did it fail on intellectual grounds (practitioners found it unconvincing)? (4) Was it never presented to popular practice (remained confined to monastic scholarship)?',
    'If suppressed by institutional gatekeeping for interest-preservation, the partition constraint exhibits snare-like extraction (institutions coordinating to maintain a false consensus). If it failed on intellectual grounds or never reached popular practice, the partition better represents genuine pluralist acceptance. The lived experience of peasant communities (who apparently did not experience the partition as coercive) suggests limited suppression, but institutional records of theological disputes suggest some active boundary-enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_suppression_mechanism, empirical, 'Causes of marginal status of unification theology despite intellectual sophistication').

omega_variable(
    meiji_reinterpretation_validity,
    'Did Meiji reformers discover that the domain partition was unstable/false, or did they impose a reinterpretation that served modernization politics (Shinto nationalism)?',
    'Comparative textual and institutional analysis: (1) Did pre-Meiji records show the partition was already under strain (theological challenges, institutional disputes)? (2) Were Meiji reforms imposed on a system that would have continued stable absent external pressure? (3) Post-reform: does separated Shinto prove partition was artificial (Shinto thrives independently), or does it prove Meiji coercion broke a functional system (Shinto declines or becomes instrumentalized)?',
    'If partition was already unstable, the constraint''s mandatrophy is genuine — the founding problem (coexistence without synthesis) was never actually solved. Meiji merely made the instability visible. If partition was genuinely stable and Meiji imposed artificial separation for political reasons, then Meiji are the external disruptors, and the constraint retains rope classification until the intervention. The interval endpoint (1868) marks the moment of rupture; the entire measurement series is pre-rupture, so this omega determines whether the rupture reveals pre-existing mandatrophy or constitutes external violence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_reinterpretation_validity, empirical, 'Whether Meiji reform revealed pre-existing instability or imposed external disruption').

omega_variable(
    peasant_theological_awareness,
    'To what degree were peasant communities aware of the theological incoherence of dual kami/Buddhist participation, and to what degree did they experience the partition as natural vs. constructed?',
    'Ethnographic and textual analysis: (1) Folk theology records, diaries, or oral traditions from peasant communities. (2) Priest-community interaction records (do priests ever explain the partition?). (3) Post-Meiji recollection: do villagers describe the partition as ''natural'' or ''something priests told us''? (4) Contemporary practice: communities still practicing both traditions without unification — is this continuity or new construction?',
    'If peasants were aware and accepted the partition as legitimate pragmatic solution, the rope classification holds from their perspective. If peasants were kept unaware and had the partition imposed through institutional authority, the constraint exhibits more snare-like characteristics (suppression of alternative understanding). The moderate accessibility_collapse (0.61) and resistance (0.48) suggest some awareness-level, but this omega distinguishes active acceptance from imposed acceptance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_theological_awareness, empirical, 'Peasant-level awareness of and agency regarding the domain partition').

omega_variable(
    kernel_reading_committer_frame,
    'Is this constraint best understood as instantiating a genuinely stable domain partition (functional pluralism), or is it a reading imposed by traditional-institutional interests to defend the status quo against theoretical critique and modernizing pressure?',
    'Genealogical analysis: Who authored the ''domain partition'' framing as an explicit doctrine? When did it become codified? Was it always the governing understanding, or did it emerge as a defense against criticism? Compare with the syncretic_fusion_reading (honji suijaku) and incoherent_bundle_reading (Meiji critique) to see which has stronger claim to represent the system''s actual operating logic vs. a retrofitted rationalization.',
    'If domain partition is a retrospective codification of incoherence, the constraint is closer to piton (institutional maintenance of a degraded functional arrangement) or snare (institutions cooperating to maintain a false consensus). If it is the genuine operating principle from the start, the rope classification holds. This omega directly addresses the committer-frame underdetermination: the domain partition reading is ONE possible framing of the kernel; the others are equally textually defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Committer-frame uncertainty: whether domain partition is genuine solution or rationalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 800, 0.25).
narrative_ontology:measurement(shin_tr_t1050, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1050, 0.37).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1300, 0.44).
narrative_ontology:measurement(shin_tr_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1550, 0.49).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1750, 0.51).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1868, 0.51).

% Extraction over time
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 800, 0.18).
narrative_ontology:measurement(shin_be_t1050, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1050, 0.28).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1300, 0.35).
narrative_ontology:measurement(shin_be_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1550, 0.39).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1750, 0.38).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1868, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t800, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 800, 0.22).
narrative_ontology:measurement(shin_su_t1050, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1050, 0.31).
narrative_ontology:measurement(shin_su_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1300, 0.38).
narrative_ontology:measurement(shin_su_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1550, 0.42).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1750, 0.42).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1868, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu_coexistence_commitment kernel has three structurally distinct readings, each with different ε values and stability predictions. Domain partition (this story) treats coexistence as stable functional separation; syncretic fusion treats it as transient stage toward theological unification; incoherent bundle treats it as unstable maintenance of false consensus. All three are competing readings of the same historical arrangement; they are linked via network.affects_constraints to mark the kernel family. The ε-invariance principle requires separate stories because the readings' core premises about the arrangement's naturalness and stability diverge, yielding different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__domain_partition_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
