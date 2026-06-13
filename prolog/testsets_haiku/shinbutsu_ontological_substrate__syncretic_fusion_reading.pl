% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Kami-Buddha Ontological Fusion (Syncretic Commitment)
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   The syncretic fusion reading asserts that kami and buddhas are
 *   ontologically unified—that honji suijaku (original essence, manifest
 *   traces) describes a genuine metaphysical truth rather than a convenience
 *   of institutional arrangement. Kami are understood as temporal
 *   manifestations of eternal buddha-natures; buddhas are the ultimate
 *   substrates of kami phenomena. This reading is ONE of three competing
 *   interpretations of the shinbutsu (kami-buddha) relationship. The fusion
 *   reading underpins integrated monastic-shrine institutional structures
 *   across medieval and early modern Japan and persists through contemporary
 *   practice. The claim/metric gap is deliberate: the constraint is CLAIMED
 *   as rope (genuine coordination solving the interpretive-plurality problem)
 *   while the authored metrics show moderate extractiveness (the fusion
 *   reading privileges integrated monastic authority over domain-partition
 *   alternatives), low theater (the commitment is internally coherent, not
 *   performative), and rising suppression (the reading's persistence
 *   increasingly depends on marginalizing domain-partition voices within
 *   institutional spaces).
 *
 * KEY AGENTS:
 *   - integrated_monastic_tradition: institutional authority, maintains the fusion interpretation within shrine-temple complexes, benefits from monopoly on legitimate theological discourse
 *   - syncretic_theological_scholars: powerful beneficiaries whose expertise and reputation rest on the validity of the fusion frame
 *   - domain_partition_advocates: moderate-power excluded voices who maintain alternative interpretations within constrained institutional spaces
 *   - state_enforcement_apparatus: institutional payer and occasional agenda-setter; historically enforced the practical fusion through administrative control
 *   - lay_practitioners: powerless beneficiary-payers; depend on the fusion frame for coherent access to integrated ritual services
 *   - empirical_historians: analytical observers questioning whether the fusion claim is metaphysically grounded or historically constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.31).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.28).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Kami-Buddha Ontological Fusion (Syncretic Commitment)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'cd1a041f-6846-4b30-98eb-a70f1fd67344').
narrative_ontology:cs_kernel_codification('cd1a041f-6846-4b30-98eb-a70f1fd67344', fixed_text).
narrative_ontology:cs_authority_grounding('cd1a041f-6846-4b30-98eb-a70f1fd67344', lineage).
narrative_ontology:cs_interpretation_layer_present('cd1a041f-6846-4b30-98eb-a70f1fd67344').
narrative_ontology:cs_reading_relation('cd1a041f-6846-4b30-98eb-a70f1fd67344', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd1a041f-6846-4b30-98eb-a70f1fd67344', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('cd1a041f-6846-4b30-98eb-a70f1fd67344', foundational, kami_buddha_metaphysical_unity).
narrative_ontology:cs_axiom_status(kami_buddha_metaphysical_unity, holdable).
narrative_ontology:cs_axiom_grounding('cd1a041f-6846-4b30-98eb-a70f1fd67344', kami_buddha_metaphysical_unity, deontological).
narrative_ontology:cs_axiom('cd1a041f-6846-4b30-98eb-a70f1fd67344', foundational, honji_suijaku_expresses_truth).
narrative_ontology:cs_axiom_status(honji_suijaku_expresses_truth, holdable).
narrative_ontology:cs_axiom_grounding('cd1a041f-6846-4b30-98eb-a70f1fd67344', honji_suijaku_expresses_truth, deontological).
narrative_ontology:cs_reference_frame('cd1a041f-6846-4b30-98eb-a70f1fd67344', unified_kami_buddha_cosmos).
narrative_ontology:cs_drift_state('cd1a041f-6846-4b30-98eb-a70f1fd67344', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cd1a041f-6846-4b30-98eb-a70f1fd67344', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, integrated_monastic_tradition).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_theological_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples and shrine complexes maintain simultaneous ritual authority over kami veneration and buddha worship as expressions of a unified metaphysical substrate. Monastery practices embed honji suijaku as the interpretive framework: kami are understood as temporary manifestations of fundamental buddha-natures, buddhas as ultimate substrates of kami phenomena. Institutional identity and theological coherence are fused with this reading.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, integrated_monastic_tradition, agenda_setter,
    institutional, generational, identity_locked, regional).

% Produce philosophical and scriptural justifications for unified kami-buddha ontology. Their scholarly authority and reputation rest on the validity of the syncretic fusion frame; departing from it would require intellectual repositioning and loss of established interpretive lineage. They benefit from the constraint's persistence because it validates their tradition's core claims.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_theological_scholars, beneficiary,
    powerful, generational, mobile, regional).

% Buddhist and Shinto practitioners who understand kami and buddhas as governing separate ontological domains (this-world prosperity vs. soteriological liberation) without fundamental fusion. They are marginalized within integrated institutional spaces and lack formal voice in determining the canonical fusion framework, though they maintain alternative practices and interpretations.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, domain_partition_advocates, excluded,
    moderate, biographical, constrained, regional).

% Examine historical evidence for when and how the syncretic fusion became codified, whether it was internally coherent or imposed through institutional power, and whether the fusion claim is grounded in genuine metaphysical commitment or institutional convenience.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, empirical_historians, observer,
    analytical, generational, analytical, global).

% At various historical moments (esp. Edo period onward), enforced the practical fusion of shrine and temple administration by law and by control of ordination and construction privileges. Bears costs of maintaining the enforcement infrastructure and administering dual-legitimacy structures. Could theoretically alter the constraint but is identity-locked to maintaining state religious coherence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, state_enforcement_apparatus, payer,
    institutional, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, state_enforcement_apparatus, agenda_setter).

% Participate in integrated shrine-temple ritual calendars and receive coordinated spiritual services (kami blessing, buddha protection, life-cycle rituals). They benefit from unified accessibility to diverse spiritual functions at single institutional nodes. They also depend on the syncretic frame for the coherence of their lived religious practice and bear indirect costs through restricted alternative frameworks.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, integrated_monastic_tradition).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining unified sacred authority over diverse spiritual phenomena (kami veneration, buddha worship, life-cycle rituals, seasonal blessings) through a single ontological and institutional framework. Practitioners can access all spiritual goods at integrated shrine-temple complexes without theological incoherence or institutional fragmentation.
% TRANSFER_FUNCTION: Transfers interpretive authority and doctrinal dominance from kami-exclusive and buddha-exclusive specialists to integrated monastic scholars who monopolize the legitimate fusion frame. Lay practitioners transfer some autonomy in religious interpretation to the unified institutional framework in exchange for coordinated access.
% ABSENT_VOICES: Domain-partition advocates (who see kami and buddhas as functionally separate) are structurally excluded from canonical theological discourse within integrated institutions. Empirical historians questioning whether the fusion is metaphysically coherent or historically constructed are treated as externalists. Practitioners whose lived experience does not require the fusion frame for coherence are not consulted on its necessity.
% DISAPPEARANCE_RATIONALE: If the syncretic fusion reading vanished and was replaced by domain-partition or incoherent-bundle readings, integrated institutions would have to reorganize: some shrine-temple complexes might separate into functionally distinct kami and buddha authorities, lay practitioners would access different ritual specialists for different needs, and the interpretive authority of integrated monastic scholars would collapse. The religious landscape would reorganize around explicit functional differentiation rather than metaphysical unity.
% FOUNDING_PROBLEM: Early medieval encounter between indigenous kami veneration and imported buddhism created interpretive plurality: how could both be true without logical contradiction or institutional duplication? The syncretic fusion solved this by positing that kami and buddhas are ontologically unified (kami as temporal manifestations of eternal buddhas), allowing a single institutional structure to serve both traditions coherently.
% FOUNDING_PROBLEM_CORROBORATION: Integrated monastic tradition and syncretic scholars attest the founding problem is live and the fusion is the correct solution. Domain-partition advocates and historians attest the problem is either falsely posed (the traditions need not be unified to coexist) or was 'solved' through institutional imposition rather than genuine metaphysical discovery. No corroboration from outside the benefiting parties exists; the constraint's persistence depends on the authority of the benefiting tradition itself.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31 endpoint) because the syncretic fusion reading operates as genuine coordination (it solves the problem of interpretive plurality and institutional coherence) but contains asymmetric authority: the integrated monastic tradition monopolizes the canonical fusion frame and marginalizes domain-partition alternatives. Suppression is moderate but rising (0.28 endpoint, upward trend 0.15→0.28) because the reading's persistence increasingly depends on actively excluding or downgrading domain-partition voices within institutional discourse—the more historians and practitioners articulate alternatives, the more institutional effort is required to maintain fusion monopoly. Theater is low (0.12) because the commitment to honji suijaku is internally coherent and philosophically sophisticated, not performative cover for institutional convenience. Accessibility collapse is high (0.78) because once the fusion reading is the canonical framework, alternatives collapse almost completely within integrated institutions—practitioners and scholars must adopt the fusion frame or exclude themselves. Resistance is moderate (0.35) because domain-partition advocates do mount real resistance, but lack institutional power to dislodge the fusion frame. The measurement series track suppression_requirement rising over the interval (0.15→0.28) as the fusion reading faces increasing empirical and philosophical challenge, requiring more institutional enforcement to maintain monopoly.
 *
 * PERSPECTIVAL GAP:
 *   The integrated monastic tradition computes the constraint as genuine coordination (rope) because it solves the interpretive-plurality problem and enables coherent institutional practice. Domain-partition advocates compute it as snare (extractive monopoly masquerading as metaphysical discovery) because it excludes their legitimate alternative and forces acceptance of fusion-authority. Empirical historians compute it as tangled_rope (real coordination with asymmetric authority) or piton (the founding coordination problem is solved, but the fusion monopoly persists theatrically through institutional inertia). The engine should compute different types for different seats from the same structural data—this is the measurement the reading-specific classification exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The integrated_monastic_tradition sits at the beneficiary end (d ~ 0.15): they set the framework, interpret it with authority, and benefit from monopoly. Syncretic_theological_scholars are beneficiaries (d ~ 0.20): their expertise and reputation depend on the fusion frame's validity. Domain_partition_advocates are targets (d ~ 0.75): they bear the cost of exclusion from canonical discourse and are suppressed by institutional authority. Lay_practitioners are near-symmetric (d ~ 0.45): they benefit from coordinated access but are constrained by the unified framework's authority and cannot articulate domain-partition alternatives without institutional friction. State_enforcement_apparatus is a payer-agenda_setter (d ~ 0.55): they bear the cost of maintaining enforcement infrastructure but retain power to alter the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The syncretic fusion reading's founding problem (how to maintain interpretive coherence when kami and buddha traditions coexist) is live but contested. The benefiting parties (integrated monastics, fusion scholars) attest it is still live and the fusion is the solution. Historians and domain-partition advocates attest the founding problem is either falsely posed (no contradiction exists if the traditions are kept functionally separate) or was 'solved' through institutional enforcement rather than genuine metaphysical discovery. This mismatch (live founding problem + contested resolution) is exactly where mandatrophy risk concentrates: if empirical scholarship establishes that the fusion was institutional imposition rather than metaphysical truth, the constraint would persist by inertia alone (theaters_ratio rising, institutional identity-lock deepening, beneficiary capture hardening). Current mandatrophy status: contested, not resolved. The constraint's theoretical foundation is under scholarly pressure; institutional persistence depends increasingly on suppression of alternatives rather than internal coherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fusion_metaphysical_vs_institutional,
    'Is the kami-buddha fusion described by honji suijaku a genuine metaphysical discovery or an institutional arrangement constructed for coherence?',
    'Historical-textual analysis of pre-fusion and early-fusion doctrine; archaeological evidence for institutional merger timelines; phenomenological study of whether fusion coherence is necessary for lay practitioners'' religious experience or imposed by monastic authority.',
    'If metaphysical: the fusion reading is intrinsically valid (rope, genuine coordination). If institutional: the reading is sustained by beneficiary authority and suppression (snare, tangled_rope, or piton). Classification shift: rope → tangled_rope or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fusion_metaphysical_vs_institutional, conceptual, 'Empirical vs. institutional status of the syncretic fusion claim.').

omega_variable(
    domain_partition_coherence,
    'Can the kami and buddha traditions coexist and function coherently WITHOUT the metaphysical fusion? Is domain-partition interpretation logically and institutionally viable?',
    'Study of contemporary Shinto communities that maintain kami practice independently of buddha frameworks; examination of lay practitioners'' actual theological coherence without monastic fusion doctrine; analysis of historical periods before fusion was institutionally enforced.',
    'If domain-partition is viable: the fusion is optional, not necessary (reduces the syncretic reading''s perceived coordination benefit, increases perceived extractiveness of the monopoly). If partition is incoherent: fusion is genuinely necessary (supports rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_coherence, empirical, 'Whether the founding coordination problem requires fusion or admits alternative solutions.').

omega_variable(
    institutional_suppression_trajectory,
    'Is the measured rise in suppression_requirement (0.15→0.28 over interval) evidence of genuine philosophical challenge to the fusion reading, or of state enforcement intensification independent of the reading''s coherence?',
    'Tracking of institutional policy changes, ordination restrictions, and doctrinal enforcement mechanisms against independent measures of scholarly critique and lay alternative practice; interview evidence from domain-partition practitioners regarding freedom to articulate alternatives.',
    'If challenge-driven: the fusion reading faces internal pressure and suppression masks declining coherence (early piton signal, mandatrophy risk rising). If enforcement-driven: suppression reflects state apparatus maintaining institutional coherence independent of the reading''s merit (pure tangled_rope with state payer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_trajectory, empirical, 'Whether suppression rise reflects philosophical challenge or institutional enforcement scaling.').

omega_variable(
    identity_lock_reversibility,
    'How deeply are integrated monastic practitioners and scholars identity-locked to the fusion reading? Would departure from the reading be experienced as identity-dissolution or as intellectual repositioning?',
    'Ethnographic study of identity-formation within integrated institutions; analysis of historical cases where practitioners shifted between fusion and partition readings; measurement of institutional exit costs for scholars and monastics who adopt domain-partition frameworks.',
    'If deeply locked: the constraint persists through identity fusion even if empirical doubts accumulate (piton risk, inertial persistence). If loosely locked: alternative readings could gain traction despite institutional resistance (rope could convert to snare more readily).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of identity-lock to the syncretic fusion frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(shin_tr_t5, observed).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(shin_tr_t10, observed).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(shin_tr_t15, observed).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(shin_tr_t20, observed).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(shin_tr_t25, observed).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(shin_tr_t30, observed).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(shin_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t5, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(shin_be_t5, observed).
narrative_ontology:measurement(shin_be_t10, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(shin_be_t10, observed).
narrative_ontology:measurement(shin_be_t15, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement_basis(shin_be_t15, observed).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement_basis(shin_be_t20, observed).
narrative_ontology:measurement(shin_be_t25, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(shin_be_t25, observed).
narrative_ontology:measurement(shin_be_t30, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(shin_be_t30, observed).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(shin_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t5, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement_basis(shin_su_t5, observed).
narrative_ontology:measurement(shin_su_t10, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement_basis(shin_su_t10, observed).
narrative_ontology:measurement(shin_su_t15, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 15, 0.24).
narrative_ontology:measurement_basis(shin_su_t15, observed).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement_basis(shin_su_t20, observed).
narrative_ontology:measurement(shin_su_t25, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 25, 0.27).
narrative_ontology:measurement_basis(shin_su_t25, observed).
narrative_ontology:measurement(shin_su_t30, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(shin_su_t30, observed).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(shin_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu_ontological_substrate kernel admits three structurally distinct readings: syncretic_fusion_reading (kami and buddhas ontologically unified via honji suijaku), domain_partition_reading (kami and buddhas govern separate domains, coexisting functionally but not metaphysically), and incoherent_bundle_reading (no coherent kernel; syncretism is accumulated institutional drift). Each reading instantiates a different constraint with different ε values, different stakeholder structures, and different computed types. The syncretic_fusion_reading (this file) describes high institutional entanglement and resistance to separation; the domain_partition_reading describes lower institutional entanglement and accepts coexistence without fusion; the incoherent_bundle_reading describes pure institutional accumulation with no underlying metaphysical commitment. All three are linked via network.affects_constraints: the fusion reading claims metaphysical truth, which directly competes with the partition reading's domain-separation claim and with the incoherent reading's denial that any coherent kernel exists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
