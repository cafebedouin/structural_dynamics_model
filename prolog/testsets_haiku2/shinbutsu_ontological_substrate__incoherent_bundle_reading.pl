% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Syncretism as Enforced Institutional Incoherence
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   Shinbutsu syncretism (kami-Buddha fusion) was institutionalized in Japan
 *   primarily through state administrative enforcement and Buddhist
 *   institutional authority claims, not through a coherent theological kernel
 *   that unified the two traditions. This reading asserts that NO such
 *   coherent kernel exists — the constraint is accumulated institutional
 *   drift under state power. Kami and Buddhist cosmologies are fundamentally
 *   incompatible (kami are locally present; Buddhist cosmology places them in
 *   eternally-absent original forms; kami govern immediate this-world
 *   protection; buddhas govern the afterlife and liberation). The state and
 *   institutional Buddhism enforced fusion not because a metaphysical
 *   unification had been discovered, but because unified administration
 *   solved state consolidation problems. Practitioners bear the cost of
 *   maintaining contradictory beliefs without access to a framework that
 *   resolves the contradiction. This contrasts sharply with the
 *   syncretic_fusion_reading (which claims honji suijaku represents genuine
 *   metaphysical unity) and the domain_partition_reading (which claims kami
 *   and buddhas naturally govern separate functional domains, making
 *   syncretism unnecessary rather than incoherent).
 *
 * KEY AGENTS:
 *   - state_apparatus: agenda-setter, enforcer, benefits from unified religious administration that prevents rival power centers
 *   - institutional_buddhism: beneficiary and partial payer, gains access to kami shrines and populations but must elaborate incoherent theology
 *   - syncretic_practitioners: powerless payers, identity-locked into maintaining contradiction without resolution
 *   - localist_kami_devotees: powerless payers, experience institutional subordination of kami to Buddhist authority
 *   - buddhist_clergy: beneficiaries and professional maintainers of the incoherence, advance careers through sophistication in defending the contradiction
 *   - intellectual_dissenters: excluded voice, would propose resolution (domain partition or kami primacy) but are suppressed by institutional control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.76).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Syncretism as Enforced Institutional Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '8869a413-93dd-4a8d-83b2-19389634eadd').
narrative_ontology:cs_kernel_codification('8869a413-93dd-4a8d-83b2-19389634eadd', distributed).
narrative_ontology:cs_authority_grounding('8869a413-93dd-4a8d-83b2-19389634eadd', extraction).
narrative_ontology:cs_interpretation_layer_present('8869a413-93dd-4a8d-83b2-19389634eadd').
narrative_ontology:cs_reading_relation('8869a413-93dd-4a8d-83b2-19389634eadd', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('8869a413-93dd-4a8d-83b2-19389634eadd', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('8869a413-93dd-4a8d-83b2-19389634eadd', foundational, no_coherent_metaphysical_kernel_exists).
narrative_ontology:cs_axiom_status(no_coherent_metaphysical_kernel_exists, holdable).
narrative_ontology:cs_axiom_grounding('8869a413-93dd-4a8d-83b2-19389634eadd', no_coherent_metaphysical_kernel_exists, empirically_contingent).
narrative_ontology:cs_axiom('8869a413-93dd-4a8d-83b2-19389634eadd', foundational, syncretism_enforced_not_discovered).
narrative_ontology:cs_axiom_status(syncretism_enforced_not_discovered, holdable).
narrative_ontology:cs_axiom_grounding('8869a413-93dd-4a8d-83b2-19389634eadd', syncretism_enforced_not_discovered, empirically_contingent).
narrative_ontology:cs_reference_frame('8869a413-93dd-4a8d-83b2-19389634eadd', state_administered_religious_unification).
narrative_ontology:cs_drift_state('8869a413-93dd-4a8d-83b2-19389634eadd', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('8869a413-93dd-4a8d-83b2-19389634eadd', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional_buddhism).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, localist_kami_devotees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, kami_shrine_administrators).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional_buddhism).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_clergy).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, kami_shrine_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposed shinbutsu syncretism administratively to consolidate previously competing kami and Buddhist authority structures under state oversight. Enforces the arrangement through shrine and temple licensing, control of ritual calendars, and institutional funding dependencies. Benefits from unified religious administration that reduces rival power centers and enables coordinated tax/labor mobilization. Could alter the arrangement at will but does not because the incoherence itself prevents practitioners from mobilizing coherently against state authority.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Gains state patronage and organizational legitimacy through syncretism, accessing shrine property networks and local populations previously outside Buddhist institutional reach. Also bears a cost: must maintain the fiction of coherent Buddhological unity with kami frameworks that are fundamentally incompatible with Buddhist metaphysics. The constraint requires continuous theological and interpretive labor (honji suijaku doctrines) to cover the contradiction without resolving it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional_buddhism, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional_buddhism, payer).

% Inherit and are required to maintain contradictory ritual and belief commitments: kami are present in shrines AND absent (relegated to eternal bodily forms in Buddhist cosmology); buddhas guide the afterlife AND kami govern immediate this-world protection. No coherent metaphysical framework integrates these claims; practitioners sustain the contradiction through performance and compartmentalization, not through resolution. Exit means abandoning family and community religious participation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Experience institutional subordination of kami devotion to Buddhist authority claims and doctrine. Their local kami cults are licensed, named, ranked, and ultimately reinterpreted through Buddhist institutional authority. They bear both the suppression of alternative kami frameworks AND the interpretive labor required to pretend the incorporation is coherent rather than imposed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, localist_kami_devotees, payer,
    powerless, biographical, identity_locked, local).

% Gain hierarchical position and funding within institutional Buddhism through mastery of syncretic theology (honji suijaku, etc.), but are obligated to defend a framework they know is incoherent. Career advancement depends on sophistication in elaborating the contradiction without resolving it. Some clergy recognize the incoherence explicitly but maintain it professionally.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_clergy, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_clergy, payer).

% Manage shrines under state and Buddhist institutional oversight. Depend on state licensing and protection; gain local prestige through shrine management. Must implement state-mandated Buddhist syncretism locally while maintaining shrine traditions, often experiencing the contradiction as administrative incoherence: rituals require non-Buddhist kami frameworks to make sense, but institutional authority demands Buddhist interpretive frames.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, kami_shrine_administrators, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, kami_shrine_administrators, beneficiary).

% Scholars and theologians (especially later Neo-Confucian and nativist thinkers) who recognize the incoherence and would argue for domain partition or kami-primacy, but are institutionally excluded from authoritative voice by state control of temple/shrine licensing and scholarly patronage. Their dissent is suppressed by the enforcement apparatus.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, intellectual_dissenters, excluded,
    powerful, biographical, trapped, national).

% Later missionary and anthropological observers who documented the arrangement and its contradictions without authority to alter it. Their accounts preserve evidence of the incoherence and the state's role in enforcing it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, colonial_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_apparatus).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies previously autonomous kami and Buddhist authority structures under centralized state administrative oversight, solving the state's coordination problem of multiple competing religious power centers. Does NOT solve a practitioners' coordination problem — the practitioners do not benefit from the unification.
% TRANSFER_FUNCTION: Moves authority, institutional legitimacy, shrine property access, and ritual interpretive power FROM localist kami practitioners TO institutional Buddhism and the state apparatus. Moves conformity costs, belief-contradiction tolerance, and interpretive labor FROM state and institutions TO practitioners who must maintain the contradiction daily.
% ABSENT_VOICES: Kami-primacy proponents (nativists) and domain-partition advocates (who later became Restoration ideologists) are structurally excluded from authoritative theological voice by state control of licensing and patronage. They would argue the arrangement is incoherent and unsustainable, that kami and Buddhism should either be genuinely integrated metaphysically or kept in separate domains, but cannot say so within institutional channels.
% DISAPPEARANCE_RATIONALE: If state enforcement of syncretism vanished, the arrangement would collapse immediately: practitioners would revert to domain-partition (kami for immediate protection, Buddhism for afterlife) or kami-primacy (as happened in the Meiji Restoration). The incoherence persists ONLY because state enforcement makes the cost of dissent higher than the cost of contradiction. Without enforcement, the contradiction becomes intolerable and alternative frameworks reorganize religious practice within years.
% FOUNDING_PROBLEM: The state faced competing religious authorities (autonomous kami cults and Buddhist institutions) that blocked centralized governance and tax collection. Syncretism solved the administrative problem by subordinating both under institutional Buddhist (state-controlled) authority.
% FOUNDING_PROBLEM_CORROBORATION: State administrative records and later Meiji-era accounts confirm the founding problem (competing authorities) was real and was solved by institutional integration. Colonial observers and later Japanese nativists and scholars attest the arrangement was incoherent; Buddhist clergy documents show deliberate theological elaboration to defend a contradiction they recognized. The Meiji Restoration's rapid dismantling of syncretism provides the strongest evidence: when state enforcement was removed, practitioners abandoned the constraint immediately and reorganized into coherent alternatives, proving the constraint's persistence depended entirely on enforcement, not on the founding problem.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is moderately high because the constraint persistently transfers authority and interpretive power FROM practitioners TO state and institutions, without providing genuine metaphysical resolution that would justify the cost. The suppression (0.76) is high because enforcement is active and necessary: without state backing and institutional Buddhist authority controls on shrine licensing and ritual calendars, practitioners would reorganize into coherent alternatives. The theater_ratio (0.62) is the highest metric, indicating the constraint's persistence depends heavily on performative maintenance — theological elaboration (honji suijaku doctrines), ritual continuity, institutional pageantry — rather than on genuine coherence or natural emergence. The high theater suggests a piton candidate, but the constraint is snare-grade because state enforcement is active (not purely theatrical) and the state materially benefits from the arrangement. The accessibility_collapse (0.48) is relatively low because alternatives (domain partition, kami primacy, pure Buddhism) remain conceptually available to anyone who steps outside the enforcement frame — the constraint does not collapse alternatives, it suppresses them. The resistance (0.71) is substantial: nativists, Confucian scholars, and later Restoration movements actively contested the arrangement and proposed alternatives, and the state had to continuously suppress these voices. The measurement series shows extractiveness accumulating over the interval (0.42→0.68) as institutional Buddhism elaborates more sophisticated theological defenses and state administrative control tightens; theater_ratio also rises (0.35→0.62), indicating the theatrical component grows relative to genuine function; suppression_requirement rises sharply (0.52→0.76) as resistance to the arrangement mounts and the state must enforce harder.
 *
 * PERSPECTIVAL GAP:
 *   The state and institutional Buddhism read syncretism as a solved coordination problem: multiple authorities unified, administration simplified, religious authority consolidated. Practitioners and localist devotees read the same arrangement as an unsolved contradiction: they must maintain incompatible belief frames without coherence, at the cost of daily cognitive dissonance and suppression of alternatives. Nativists and later Confucian critics would read it as an imposed arrangement that suppresses what could be coherent alternatives (domain partition, kami primacy). The gap reflects fundamental asymmetry in power (state/institutions can opt out; practitioners cannot) and in benefit (state gains administrative control; practitioners gain nothing except the obligation to conform). The engine computes per-seat types from the structural positions: the state's seat should compute as something closer to rope (coordination benefit + active enforcement it controls); the practitioners' seats compute as snare (target position, suppressed alternatives, enforced incoherence).
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus (institutional power, analytical time horizon) benefits from the arrangement and could alter it — directionality near the beneficiary end (d ≈ 0.1). Institutional Buddhism (institutional power) also benefits (access to shrines and populations) but pays a cost (must elaborate incoherent theology to defend the arrangement). Their directionality is slightly toward the target (d ≈ 0.35–0.40) because they are partially captured by the requirement to maintain the fiction. Syncretic practitioners and localist kami devotees (powerless, identity_locked exit) are full targets: they cannot exit without abandoning community and must maintain the contradiction daily. Their directionality is near the target end (d ≈ 0.85–0.95). Buddhist clergy (powerful, constrained exit due to career dependence on institutional Buddhism) are mixed: they benefit professionally but are obligated to defend what many recognize as incoherence. Directionality around d ≈ 0.45. Intellectual dissenters (excluded by enforcement) would have directionality toward beneficiary (they would benefit from alternatives) but are suppressed before their position registers in the constraint structure. The seat divergence is sharp: from the state's position the arrangement solves a genuine coordination problem (unifying competing authorities); from the practitioners' position the same structure operates as enforced maintenance of contradiction. This divergence is the core of the snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competing religious authorities blocking state consolidation) was genuine at the time of syncretism's institutionalization. However, the interval's measurement data shows the founding problem was largely solved by institutional integration within the first 10–15 time units, yet the constraint persisted and intensified (extractiveness continued rising). This is the mandatrophy signature: the original mandate (solve the coordination problem of unifying authorities) was achieved, but the constraint persisted because state and institutional Buddhism had acquired structural benefits from it (tax leverage, ritual control, cultural authority) independent of the founding problem. The rising theater_ratio (0.35→0.62) and continued suppression (0.52→0.76) despite the founding problem becoming moot indicate a zombie constraint sustained by inertia and active enforcement, not by ongoing coordination need. The constraint should be classified with mandatrophy_resolved: true, and the historical record (Meiji Restoration's rapid dismantling of syncretism) confirms the assessment — when state enforcement was removed, the arrangement collapsed immediately, showing it had never become self-sustaining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_vs_institutional_performance,
    'Is the incoherence ITSELF a functional property of state control (making resistance diffuse by making belief irresolvable), or is the incoherence a side effect of imposed syncretism that persists because state enforcement prevents practitioners from reorganizing?',
    'Comparative analysis of state benefit under coherent vs. incoherent religious arrangements; analysis of state response when intellectuals and later Restoration movements proposed resolving the incoherence in specific directions.',
    'If incoherence is functional-intentional, the constraint is more purely extractive (state benefits from confusion itself). If incoherence is a side effect, extraction is concentrated on institutional control rather than cognitive disruption, and the snare''s grip weakens faster if enforcement slackens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_institutional_performance, empirical, 'Whether state extracted value from the contradiction itself or from the institutional consolidation the contradiction enabled.').

omega_variable(
    practitioners_belief_compartmentalization,
    'Do syncretic practitioners genuinely believe contradictory claims are coherent (compartmentalized resolution they teach themselves), or do they understand the contradiction and accept it as the cost of participation?',
    'Ethnographic and autobiographical evidence from practitioners about how they reconcile the beliefs; analysis of which kami-related and Buddhist-related moments invoke which framework and whether practitioners experience switching as resolution or as contradiction-management.',
    'If compartmentalization is sincere, practitioners extract psychological benefit from the arrangement (belief unification, however false). If the contradiction is consciously maintained, suppression is higher (practitioners knowingly sustain a false frame) and identity-locking is more severe (they must maintain an incoherence they recognize).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practitioners_belief_compartmentalization, empirical, 'Whether practitioners experience the syncretism as resolved or as tolerated contradiction.').

omega_variable(
    reading_contest_kernel_identity,
    'This reading asserts NO coherent kernel exists — only accumulated institutional drift. The syncretic_fusion_reading asserts a unified metaphysical kernel (honji suijaku). The domain_partition_reading asserts two functional kernels. Are these three readings of ONE contested kernel, or do they describe THREE incommensurable constraint structures?',
    'Structural analysis: does the state enforce the same arrangement (shinbutsu syncretism as administrative policy) across all three readings, or does each reading refer to a different historical arrangement? If state enforcement is the fixed point and theological interpretation varies, they are readings of one kernel. If the historical arrangements differ, they are three separate constraints with no kernel contest.',
    'If they are genuine readings of one kernel, the contest is over whether the kernel is coherent (fusion vs. partition vs. incoherent-bundle). If they describe different arrangements, the ''kernel'' label is a linguistic artifact and the framework should decompose them as three separate constraint stories linked by network relationships rather than kernel-reading relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether the three readings contest a single kernel or describe separate constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shin_be_t5, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(shin_be_t10, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(shin_be_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(shin_be_t25, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(shin_be_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(shin_su_t5, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(shin_su_t10, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(shin_su_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(shin_su_t25, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement(shin_su_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__domain_partition_reading).

% DUAL FORMULATION NOTE:
% Three readings of one contested kernel: incoherent_bundle (this story) asserts no coherent metaphysical kernel unifies kami and Buddhism, only state-enforced institutional drift. syncretic_fusion asserts honji suijaku reveals genuine metaphysical unity. domain_partition asserts kami and buddhas govern separate functional domains naturally. All three contest the same historical arrangement; the readings differ on whether that arrangement instantiates a real kernel or merely performs coherence. The incoherent_bundle reading produces the highest ε (0.68) because coherence-without-foundation is a pure extraction structure; the fusion and partition readings produce lower ε if their metaphysical claims are accepted as true. Network links represent reading-level influence: the incoherent_bundle reading, if accepted, makes both fusion and partition claims look like post-hoc rationalization of an arbitrary arrangement rather than genuine discovery of kernel truth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
