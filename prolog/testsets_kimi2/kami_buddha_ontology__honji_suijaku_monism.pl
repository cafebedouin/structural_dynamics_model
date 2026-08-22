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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Honji Suijaku Monism â Kami as Buddhist Traces
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   The honji suijaku monism reading of the kami-buddha ontology kernel holds
 *   that kami are phenomenal traces (suijaku) of the original Buddhist ground
 *   (honji). Systematized by Tendai and Shingon lineages during the Heian
 *   period, it became the dominant theological framework of medieval Japanese
 *   religion. It is one reading of a contested kernel; siblings include
 *   domain_partition (separate functional domains) and incoherent_bundle
 *   (institutionally sustained contradiction). This reading instantiates a
 *   hierarchical ontology with Buddhist entities as prior and kami as
 *   dependent manifestations, extracting theological autonomy from shrine
 *   cults while coordinating a unified ritual economy.
 *
 * KEY AGENTS:
 *   - esoteric_buddhist_lineages: Primary agenda-setter and extraction capturer (institutional/identity_locked) â systematizes the doctrine, enforces it through jinguji complexes, and collects ritual authority and patronage.
 *   - shrine_priesthoods: Primary payer (organized/constrained) â administer kami worship under Buddhist theological subordination, losing independent ritual authority.
 *   - aristocratic_patrons: Coordination beneficiary (powerful/mobile) â gain unified ritual coverage across both traditions without sectarian conflict.
 *   - rural_kami_communities: Diffuse payer (powerless/trapped) â local worship is reinterpreted through Buddhist liturgy without theological agency.
 *   - nativist_shinto_scholars: Excluded voice (moderate/constrained) â would assert kami independence but are marginalized from institutional discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.76).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.82).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.76).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism â Kami as Buddhist Traces").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious/philosophical").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'c84a92b2-092a-4ab9-b74d-ed26c7b3813a').
narrative_ontology:cs_kernel_codification('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', formalized).
narrative_ontology:cs_authority_grounding('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', lineage).
narrative_ontology:cs_interpretation_layer_present('c84a92b2-092a-4ab9-b74d-ed26c7b3813a').
narrative_ontology:cs_reading_relation('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', foundational, honji_prior_to_suijaku).
narrative_ontology:cs_axiom_status(honji_prior_to_suijaku, holdable).
narrative_ontology:cs_axiom_grounding('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', honji_prior_to_suijaku, theological).
narrative_ontology:cs_axiom('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', foundational, kami_lack_independent_swabhava).
narrative_ontology:cs_axiom_status(kami_lack_independent_swabhava, holdable).
narrative_ontology:cs_axiom_grounding('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', kami_lack_independent_swabhava, theological).
narrative_ontology:cs_reference_frame('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', classical_honji_suijaku_system).
narrative_ontology:cs_drift_state('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', edo_nativist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c84a92b2-092a-4ab9-b74d-ed26c7b3813a', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, esoteric_buddhist_lineages).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, aristocratic_patrons).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shrine_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, rural_kami_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Systematized honji suijaku doctrine within Tendai and Shingon traditions, installed it as the authoritative framework for interpreting kami, and administered the jinguji shrine-temple complexes. Collected land, patronage, and ritual supremacy by subordinating indigenous deities to Buddhist bodhisattvas. Their institutional identity is fused with the doctrine; abandoning it would dissolve their theological hegemony.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, esoteric_buddhist_lineages, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, esoteric_buddhist_lineages, beneficiary).

% Administered kami shrines under the doctrinal framework that their deities were local manifestations of Buddhist honji. Performed Buddhist rites and accepted Buddhist iconography to maintain institutional funding and aristocratic connections, forfeiting independent theological authority and the ability to assert kami primacy without risking patronage loss.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shrine_priesthoods, payer,
    organized, biographical, constrained, national).

% Commissioned rituals from the unified shrine-temple system for both this-worldly benefits (kami) and salvation (buddhas). Benefited from a single cosmological framework that allowed simultaneous patronage without requiring theological resolution of competing claims; their social and religious needs were covered by one integrated ritual economy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, aristocratic_patrons, beneficiary,
    powerful, biographical, mobile, national).

% Maintained local kami worship at village shrines that were progressively absorbed into the jinguji network. Their festivals, prayers, and sacred landscapes were reinterpreted through Buddhist liturgy and ontology; they possessed no theological leverage to resist the reframing and no alternative ritual infrastructure outside the temple-shrine complex.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, rural_kami_communities, payer,
    powerless, immediate, trapped, local).

% Scholars and priests who asserted the independent divinity and priority of kami against Buddhist subordination. During the hegemony of honji suijaku they were marginalized from official theological discourse, denied state and aristocratic patronage, and confined to private academies or secret lineages; their objections were not admitted into the institutional conversation.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, nativist_shinto_scholars, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, esoteric_buddhist_lineages).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles the simultaneous worship of kami and buddhas in Japanese religious life by subsuming both under a single hierarchical ontology, enabling joint ritual practice, shared patronage networks, and a unified cosmological account of this-worldly and other-worldly power.
% TRANSFER_FUNCTION: Moves ontological priority, ritual authority, and economic patronage from independent shrine cults and their communities to Buddhist temple institutions; reclassifies kami as local phenomenal traces of universal Buddhist grounds.
% ABSENT_VOICES: Pure Shinto theologians and nativist scholars who would assert the independent divinity and native priority of kami were structurally excluded from official discourse during the hegemony of the honji suijaku system; their positions survive only in marginal texts, later revival movements, and the kokugaku tradition.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the jinguji system would lose its theological foundation; shrine-temple complexes would separate or reorganize around distinct ritual economies; aristocratic patronage would split between Buddhist and Shinto institutions; and Buddhist lineages would lose their claim to mediate and subsume kami worship.
% FOUNDING_PROBLEM: The coexistence of indigenous kami worship and imported Buddhism in Japan created theological tension, institutional competition over ritual patronage, and cosmological confusion about the relationship between local and universal sacred power.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist temples attest the problem required ontological unification through honji suijaku. Shinto revivalists and modern historians attest the problem was real but was resolved by subordinating one tradition to the other rather than by genuine synthesis. Corroboration from outside the benefiting parties comes from kokugaku scholars and academic historians of Japanese religion.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.76, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.76) is high because the doctrine transfers ontological priority and economic patronage decisively to Buddhist institutions. Suppression (0.82) is higher: the constraint persists only through active enforcement â jinguji fusion, state support, and the exclusion of rival Shinto theologies. Theater ratio (0.55) reflects that by the late period, much activity was performative maintenance of a hierarchy whose theological justification was increasingly questioned. Accessibility collapse (0.78) is high because alternative ontologies (pure Shinto, domain partition) became institutionally unthinkable within the temple-shrine complex. Resistance (0.48) is moderate: nativist movements objected but were suppressed until the Meiji separation. Metrics and claim are authored independently: I claim tangled_rope because a genuine coordination function (unifying two ritual economies) is inseparable from asymmetric extraction (subordinating shrines to temples).
 *
 * PERSPECTIVAL GAP:
 *   The esoteric Buddhist lineages experience this constraint as genuine theological insight and necessary coordination; the shrine priesthoods experience it as institutional subordination dressed in doctrinal language. The aristocratic patrons experience a convenient symmetry. The engine computes this divergence from the structural data â same constraint, opposed directionalities depending on whether the agent subsidizes or is extracted from.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist lineages are structural beneficiaries (low d): the constraint subsidizes their authority and channels patronage to them. Shrine priesthoods and rural communities are structural payers (high d): the constraint extracts their theological autonomy and reinterprets their objects of worship. Aristocratic patrons sit near symmetric (moderate d): they benefit from coordination but do not capture the extraction. Nativist scholars are excluded (analytical/high d in potential). The derivation follows from beneficiary/victim declarations and exit modulation: agenda-setters with identity_locked exit sit nearer the beneficiary end than their global power would predict, while constrained shrine priests sit nearer the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â reconciling kami and buddha worship â was arguably live in the ninth century but was long dead by the Edo period, when the arrangement persisted primarily to maintain Buddhist institutional dominance. The R5 genealogy interview records this as contested. If the engine computes founding_problem_status=dead paired with disappearance_verdict=world_rearranges, the mandatrophy flag fires: the constraint has outlived its function and persists by inertia or extraction. The theater_ratio (0.55) and rising extractiveness series support this diagnostic without forcing the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_institutional_construction,
    'Is the honji suijaku relation an ontological fact about kami and buddhas, or a doctrinal construction developed to secure Buddhist institutional dominance?',
    'Comparative historical analysis of non-Japanese Buddhist cultures without this specific ontology, and statistical analysis of the correlation between doctrinal codification and land or patronage transfers to temples.',
    'If purely constructed, classification shifts toward snare (the coordination story is cover for extraction); if genuinely believed theological truth with extraction as side effect, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_institutional_construction, conceptual, 'Ontological reality versus ideological construction of the honji suijaku hierarchy').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the persistence of honji suijaku achieved through state-temple coercion, or through internalized belief by shrine priests?',
    'Analysis of Edo-period shrine records to determine whether priests maintained Buddhist rites out of conviction or economic necessity; post-Meiji separation trajectory to see if Shinto priests immediately abandoned the framework or retained elements.',
    'If internalized, effective suppression is higher than the structural measure suggests; if purely structural, collapse should be rapid once coercion is removed (Meiji separation supports rapid collapse).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in shrine-temple relations').

omega_variable(
    coordination_without_hierarchy,
    'Could the coordination function â reconciling kami and buddha worship â have been achieved without the hierarchical subordination of kami to buddhas?',
    'Cross-cultural comparison with religious syncretisms that achieved integration without ontological priority, and analysis of medieval Japanese texts for non-hierarchical alternatives that were historically proposed but suppressed.',
    'If coordination without hierarchy was historically possible, the extraction component is larger than necessary and the constraint moves toward snare; if hierarchy was structurally necessary for the coordination, the tangled_rope classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_without_hierarchy, conceptual, 'Whether hierarchical subordination was necessary for the coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honji_suijaku_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.18).
narrative_ontology:measurement(honji_suijaku_tr_t8, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 8, 0.25).
narrative_ontology:measurement(honji_suijaku_tr_t16, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 16, 0.32).
narrative_ontology:measurement(honji_suijaku_tr_t24, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 24, 0.4).
narrative_ontology:measurement(honji_suijaku_tr_t32, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 32, 0.46).
narrative_ontology:measurement(honji_suijaku_tr_t40, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 40, 0.51).
narrative_ontology:measurement(honji_suijaku_tr_t50, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(honji_suijaku_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(honji_suijaku_be_t8, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(honji_suijaku_be_t16, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(honji_suijaku_be_t24, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(honji_suijaku_be_t32, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(honji_suijaku_be_t40, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 40, 0.73).
narrative_ontology:measurement(honji_suijaku_be_t50, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 50, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(honji_suijaku_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(honji_suijaku_su_t8, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(honji_suijaku_su_t16, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(honji_suijaku_su_t24, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(honji_suijaku_su_t32, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(honji_suijaku_su_t40, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(honji_suijaku_su_t50, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, incoherent_bundle).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel decomposes into three structurally distinct constraints: domain_partition (separate domains, lower extraction), honji_suijaku_monism (hierarchical monism, high extraction with coordination), and incoherent_bundle (bundle of contradictions, high theater). This story is the monism reading; it structurally influenced both siblings by suppressing domain_partition and providing the primary doctrinal content that incoherent_bundle deconstructs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
