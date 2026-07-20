% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism: Kami as Phenomenal Traces of Buddhist Original Ground
 *   domain: religious/philosophical/cultural_history
 *
 * SUMMARY:
 *   The honji-suijaku doctrine asserts that kami are phenomenal traces
 *   (suijaku) of the original Buddhist ground (honji), typically identified
 *   with the Dharmakaya or specific buddhas and bodhisattvas. Emerging in the
 *   Heian period and elaborated by Tendai and Shingon theologians, this
 *   reading of the kami-buddha relationship became the dominant theological
 *   framework of medieval Japanese Shinbutsu-shugo. It is ONE reading of a
 *   contested kernel: siblings include domain_partition (separate functional
 *   spheres) and incoherent_bundle (no coherent kernel, only institutional
 *   bundling). This constraint story models the structural properties of the
 *   monistic reading alone, per the Îµ-invariance principle.
 *
 * KEY AGENTS:
 *   - buddhist_temple_network: Primary agenda-setter (institutional/constrained) â propagates the doctrine, administers shrine-temple multiplexes, and collects patronage and land rents justified by ontological priority
 *   - shinto_priesthood: Primary payer (organized/identity_locked) â performs kami rituals under Buddhist interpretive authority, denied independent theological canon
 *   - syncretic_theologians: Beneficiary (moderate/constrained) â intellectual laborers who systematize correspondence charts and receive ecclesiastical patronage
 *   - local_kami_communities: Secondary payer (powerless/identity_locked) â village worship groups whose local deities are reinterpreted as Buddhist traces
 *   - rival_pure_land_shintoists: Excluded (powerless/trapped) â advocates of exclusive kami worship structurally barred from canonical discourse and patronage networks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.62).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism: Kami as Phenomenal Traces of Buddhist Original Ground").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious/philosophical/cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'c9df9227-3314-41de-a9e5-bd85ef44cecb').
narrative_ontology:cs_kernel_codification('c9df9227-3314-41de-a9e5-bd85ef44cecb', fixed_text).
narrative_ontology:cs_authority_grounding('c9df9227-3314-41de-a9e5-bd85ef44cecb', lineage).
narrative_ontology:cs_interpretation_layer_present('c9df9227-3314-41de-a9e5-bd85ef44cecb').
narrative_ontology:cs_reading_relation('c9df9227-3314-41de-a9e5-bd85ef44cecb', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('c9df9227-3314-41de-a9e5-bd85ef44cecb', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('c9df9227-3314-41de-a9e5-bd85ef44cecb', foundational, kami_are_phenomenal_traces).
narrative_ontology:cs_axiom_status(kami_are_phenomenal_traces, holdable).
narrative_ontology:cs_axiom_grounding('c9df9227-3314-41de-a9e5-bd85ef44cecb', kami_are_phenomenal_traces, theological).
narrative_ontology:cs_axiom('c9df9227-3314-41de-a9e5-bd85ef44cecb', foundational, buddhist_dharmakaya_primacy).
narrative_ontology:cs_axiom_status(buddhist_dharmakaya_primacy, holdable).
narrative_ontology:cs_axiom_grounding('c9df9227-3314-41de-a9e5-bd85ef44cecb', buddhist_dharmakaya_primacy, theological).
narrative_ontology:cs_reference_frame('c9df9227-3314-41de-a9e5-bd85ef44cecb', buddhist_ontological_primacy).
narrative_ontology:cs_drift_state('c9df9227-3314-41de-a9e5-bd85ef44cecb', edo_kokugaku_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c9df9227-3314-41de-a9e5-bd85ef44cecb', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_temple_network).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, syncretic_theologians).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_priesthood).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, local_kami_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major Buddhist temples and sects (Tendai, Shingon, and later others) propagated the honji-suijaku framework nationwide through shrine-temple multiplexes. They collected land, imperial patronage, and ritual authority by positioning themselves as the ontological ground interpreting kami. Their institutional identity was fused with the doctrine; abandoning it would dissolve their claimed right to oversee indigenous cults.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_temple_network, agenda_setter,
    institutional, generational, constrained, national).

% Shrine priests performed kami rituals but were increasingly required to interpret their deities through Buddhist categories, install Buddhist chapels on shrine grounds, and accept clerical oversight from branch temples. Their theological autonomy was subordinated to Buddhist scholastic authority, though they retained local liturgical roles. Exit would have required abandoning their priestly identity or facing institutional exclusion.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_priesthood, payer,
    organized, generational, identity_locked, national).

% Scholars and monks who produced honji-suijaku correspondence charts and doctrinal literature mapping specific kami to specific buddhas and bodhisattvas. Their careers, patronage, and scholarly reputations depended on the doctrine's continued acceptance by temple networks and the court.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, syncretic_theologians, beneficiary,
    moderate, biographical, constrained, national).

% Village communities and local worship groups who maintained agricultural rites, festivals, and ancestral kami practices. Their deities were reinterpreted as traces of Buddhist reality, and their rituals were required to incorporate Buddhist elements. They experienced the constraint as the inability to honor kami as fully independent powers without Buddhist overlay.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, local_kami_communities, payer,
    powerless, biographical, identity_locked, local).

% Advocates of exclusive kami worship in rural or peripheral regions who rejected Buddhist theological overlay. They were structurally excluded from canonical discourse, denied state and temple patronage, and risked having their shrines absorbed into temple networks. Their voices do not appear in the honji-suijaku scholastic literature.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, rival_pure_land_shintoists, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, buddhist_temple_network).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates Buddhism and indigenous kami worship into a single hierarchical theological framework, resolving the potential conflict of two coexisting religious systems by assigning them differentiated ontological ranks within one overarching reality.
% TRANSFER_FUNCTION: Transfers ontological priority, interpretive authority, and institutional patronage from kami-cult actors and local shrines to Buddhist temples and theologians; kami retain devotional practice but lose independent ultimate status.
% ABSENT_VOICES: Shrine priests advocating for kami theological independence, practitioners of exclusively local kami cults without Buddhist overlay, and later kokugaku scholars who rejected Buddhist interpretive dominance are structurally absent from the canonical formulation.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, the medieval and early-modern shrine-temple multiplex system would lose its hierarchical justification, Buddhist oversight of kami rituals would dissolve, and the theological basis for subordinating indigenous deities to imported categories would collapse, forcing a reorganization of Japanese religious institutional relationships.
% FOUNDING_PROBLEM: How to reconcile the simultaneous presence and authority of Buddhism and indigenous kami worship in Japan without theological contradiction or destabilizing institutional competition between temples and shrines.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary religious historians and Buddhist studies scholars outside the benefiting tradition attest that the integration problem was historically real in the Nara and Heian periods. However, Shinto studies scholars and kokugaku historiography attest that the problem was resolved by later separation rather than by permanent Buddhist dominance, and that the arrangement persisted by inertia after the original syncretic pressure had dissipated.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored at moderate-high because the doctrine systematically reallocates ontological priority and institutional authority from Shinto actors to Buddhist entities, though material extraction is indirect (patronage and land rather than tribute). Suppression (0.62) reflects the institutional mechanisms â shrine-temple multiplexes, doctrinal policing, and patronage control â that prevented rival readings from achieving canonical status. Theater_ratio (0.40) acknowledges genuine theological content while registering that an increasing share of later practice was performative maintenance of a settled hierarchy. Accessibility_collapse (0.72) is high because within the accepted Buddhist episteme, the suijaku status of kami appears as natural consequence rather than contested interpretation. Resistance (0.48) registers persistent but institutionally muted opposition from Shinto priests and the eventual kokugaku challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist temple seat, honji-suijaku is a gracious elevation of kami into the Buddhist cosmic hierarchy â a revelation that coordinates two traditions by revealing their deeper unity. From the Shinto priesthood seat, it is a unilateral ontological capture that subordinates indigenous deities to foreign categories and extracts ritual autonomy. From the analytical seat, the same structural arrangement produces both experiences simultaneously; the engine computes the divergence without adjudicating which perception is true.
 *
 * DIRECTIONALITY LOGIC:
 *   The buddhist_temple_network sits near the beneficiary pole (low d): the constraint subsidizes their authority by providing the theological justification for overseeing kami cults. Syncretic_theologians also sit near the beneficiary pole, though less extremely, as their careers depend on the doctrine but they do not directly collect institutional rents. The shinto_priesthood and local_kami_communities sit near the target pole (high d): the constraint extracts their ontological autonomy and re-routes it through Buddhist interpretive categories. The excluded rival_pure_land_shintoists sit at the extreme target end, as their total exclusion means the constraint extracts existence itself from their theological position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â how to integrate Buddhism and kami worship â was genuinely live in the Nara and Heian periods. By the Edo period, however, the problem had been replaced by the challenge of maintaining Buddhist institutional dominance in a society where the syncretic framework had become naturalized. The R5 genealogy records founding_problem_status: dead, and the temporal measurements show rising theater_ratio alongside declining base_extractiveness and suppression_requirement in late phases, consistent with mandatrophy. The authored type captures the active-phase structure as tangled_rope; the late-phase drift toward piton is recorded in the measurement series without forcing claim migration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructed_doctrine_vs_ontological_truth,
    'Is the honji-suijaku framework a trans-historical ontological truth about the identity of kami and buddhas, or a historically contingent ideological construct developed to secure Buddhist institutional dominance over indigenous cults?',
    'Comparative historical analysis of parallel syncretic formations outside Japan (e.g., Buddhist-Bon interactions in Tibet, Catholic-saint syncretism in the Americas) to determine whether the structural pattern is better explained by theological discovery or by institutional power dynamics.',
    'If shown to be primarily constructed, the constraint''s extractiveness is confirmed as politically interested rather than cognitively necessary; if shown as ontologically true within its framework, the tangled_rope classification may need revision toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_doctrine_vs_ontological_truth, conceptual, 'Ambiguity between theological truth claim and ideological construction').

omega_variable(
    sibling_reading_boundary,
    'Does the honji-suijaku monism reading genuinely foreclose the domain_partition reading within a single logical framework, or do these readings operate at different analytical registers (ontological vs. functional) such that they are not strict contradictories?',
    'Examine whether medieval Japanese actors simultaneously held both frameworks functionally (separate rituals for birth vs. death) while asserting monism doctrinally; if so, the foreclosure relation is analytical rather than historical.',
    'If the readings are not genuine logical contradictories, the forecloses relation in cs_structure should be revised to coexists_with, altering the kernel''s contamination network topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Uncertainty about the logical relationship between monism and domain partition readings').

omega_variable(
    enforcement_mechanism_decay,
    'Did the constraint''s suppressive capacity decay because enforcement institutions weakened, or because the constraint became internalized into the self-concept of subordinated actors (Shinto priests accepting their suijaku status as natural)?',
    'Trace post-Meiji shinbutsu bunri behavior: if Shinto priests immediately reclaimed independent theological status after external enforcement was removed, suppression was primarily structural; if independence was difficult even after legal separation, suppression was partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint exhibits stronger cognitive-capture features; if purely structural, the constraint is a conventional tangled_rope with institutional enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_decay, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kami_tr_t150, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 150, 0.28).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 300, 0.32).
narrative_ontology:measurement(kami_tr_t450, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 450, 0.38).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 600, 0.45).
narrative_ontology:measurement(kami_tr_t750, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 750, 0.55).
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 900, 0.65).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kami_be_t150, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 150, 0.55).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 300, 0.63).
narrative_ontology:measurement(kami_be_t450, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 450, 0.6).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 600, 0.52).
narrative_ontology:measurement(kami_be_t750, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 750, 0.45).
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 900, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(kami_su_t150, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 150, 0.55).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 300, 0.65).
narrative_ontology:measurement(kami_su_t450, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 450, 0.7).
narrative_ontology:measurement(kami_su_t600, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 600, 0.6).
narrative_ontology:measurement(kami_su_t750, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 750, 0.45).
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 900, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kami_buddha_ontology kernel. The kernel decomposes into multiple structurally distinct constraints because the label Shinbutsu-shugo conflates ontological claims (this reading), functional separation (domain_partition), and institutional critique (incoherent_bundle). Each reading carries a distinct epsilon, beneficiary/victim structure, and stakeholder surface.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
