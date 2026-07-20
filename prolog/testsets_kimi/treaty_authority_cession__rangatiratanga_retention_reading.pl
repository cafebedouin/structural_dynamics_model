% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty of Waitangi â Rangatiratanga Retention Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint story instantiates the rangatiratanga_retention_reading
 *   of the treaty_authority_cession kernel. Under this reading, the MÄori
 *   text of the Treaty of Waitangi controls via contra proferentem, limiting
 *   kÄwanatanga to governance and retaining tino rangatiratanga in hapÅ«.
 *   The treaty establishes a partnership requiring ongoing consent. While the
 *   reading presents as coordination, its historical embedding in colonial
 *   land alienation and the Crown's retention of parliamentary sovereignty
 *   create asymmetric extraction. The Crown is the primary beneficiary of
 *   legitimacy and orderly governance; MÄori hapÅ« and iwi are the
 *   structural targets, coordinated into a partnership that nevertheless
 *   preserves Crown supremacy. A retrospective snare is visible in the land
 *   transfers executed under the English cession reading, which this reading
 *   exposes but does not fully remedy.
 *
 * KEY AGENTS:
 *   - crown_government: Agenda setter / beneficiary (institutional/constrained) â retains parliamentary sovereignty while claiming partnership legitimacy
 *   - maori_hapu_and_iwi: Payer / coordinated target (organized/identity_locked) â retain tino rangatiratanga nominally but bear costs of legislative override and unresolved alienation
 *   - waitangi_tribunal: Observer (institutional/analytical) â validates MÄori text reading without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.58).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.62).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty of Waitangi â Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, 'f14754e5-7afe-48a8-911c-49b2478e8525').
narrative_ontology:cs_kernel_codification('f14754e5-7afe-48a8-911c-49b2478e8525', fixed_text).
narrative_ontology:cs_authority_grounding('f14754e5-7afe-48a8-911c-49b2478e8525', lineage).
narrative_ontology:cs_interpretation_layer_present('f14754e5-7afe-48a8-911c-49b2478e8525').
narrative_ontology:cs_reading_relation('f14754e5-7afe-48a8-911c-49b2478e8525', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('f14754e5-7afe-48a8-911c-49b2478e8525', treaty_authority_cession__biculturalism_reading, coexists_with).
narrative_ontology:cs_axiom('f14754e5-7afe-48a8-911c-49b2478e8525', foundational, maori_text_authoritative).
narrative_ontology:cs_axiom_status(maori_text_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('f14754e5-7afe-48a8-911c-49b2478e8525', maori_text_authoritative, conventional).
narrative_ontology:cs_axiom('f14754e5-7afe-48a8-911c-49b2478e8525', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('f14754e5-7afe-48a8-911c-49b2478e8525', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_reference_frame('f14754e5-7afe-48a8-911c-49b2478e8525', tino_rangatiratanga_intact_partnership).
narrative_ontology:cs_drift_state('f14754e5-7afe-48a8-911c-49b2478e8525', contemporary_parliamentary_sovereignty_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f14754e5-7afe-48a8-911c-49b2478e8525', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_government).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, maori_hapu_and_iwi).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the treaty partnership framework; claims limited kÄwanatanga contingent on ongoing hapÅ« consent; collects governance legitimacy and orderly settlement of historical claims; enforces parliamentary statutes that retain ultimate override authority.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_government, agenda_setter,
    institutional, generational, constrained, national).

% Bear the structural cost of Crown sovereignty retention despite treaty guarantees of tino rangatiratanga; participate in tribunal and settlement processes that recognize but do not fully restore chiefly authority; exit to independent self-determination is blocked by Crown monopoly on law and violence, while political identity remains fused with whakapapa to the signatories.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, maori_hapu_and_iwi, payer,
    organized, generational, identity_locked, national).

% Investigates treaty breaches and produces authoritative reports affirming the MÄori text reading; issues recommendations that the Crown may accept or ignore; provides analytical validation without enforcement capacity.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, crown_government).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates authority-sharing between the Crown and MÄori by limiting Crown kÄwanatanga to governance functions, reserving tino rangatiratanga to hapÅ«, and replacing unilateral sovereignty with a partnership requiring ongoing negotiation and consent.
% TRANSFER_FUNCTION: Transfers legitimacy and jurisdictional scope from absolute Crown sovereignty to a conditional, consent-based partnership; historically transferred land and authority from MÄori to Crown under the English cession reading, which this reading seeks to arrest and remediate.
% ABSENT_VOICES: PÄkehÄ electorate majorities who oppose co-governance are structurally absent from the MÄori-text interpretive room; the English-reading judiciary and colonial administrators who originally enforced land confiscation are not present in the partnership framework, though their legal handiwork persists in the property regime.
% DISAPPEARANCE_RATIONALE: If the rangatiratanga retention reading vanished overnight, the Waitangi Tribunal would lose its foundational authority, treaty settlements would revert to ordinary Crown grants, and MÄori authority would collapse to the English cession framework â the constitutional order would reorganize around unilateral Crown sovereignty.
% FOUNDING_PROBLEM: The need to establish a lawful British presence in New Zealand while securing MÄori agreement to limited governance, avoiding armed conflict and creating a basis for inter-community order.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians and the Waitangi Tribunal attest that the MÄori text did not cede sovereignty; British Colonial Office correspondence indicates Crown drafters were aware of textual divergence. No impartial contemporary party fully corroborates the Crown's narrative of completed cession.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58) reflects the mid-high asymmetry of a partnership where Crown sovereignty remains override-capable despite treaty guarantees. Suppression (0.62) encodes the structural unavailability of full MÄori legal independence within the NZ state. Theater ratio (0.48) captures the performative aspect of contemporary partnership rhetoric that outruns institutional power-sharing. Accessibility collapse (0.45) indicates that while MÄori statehood is conceptually live, it is legally foreclosed. Resistance (0.70) reflects sustained MÄori political and legal mobilization. Temporal measurements show extraction declining from colonial highs (0.90) as the reading gains institutional traction, but stabilizing well above zero because parliamentary sovereignty is not relinquished.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat, the constraint is a coordination mechanism that legitimizes governance and enables orderly redress. From the MÄori seat, the same structure is an incomplete restoration: it coordinates recognition but extracts authority by preserving Crown supremacy. The engine will compute the Crown seat as low-directionality beneficiary and the MÄori seat as high-directionality target, producing divergent per-seat classifications despite the shared partnership vocabulary.
 *
 * DIRECTIONALITY LOGIC:
 *   crown_government is declared beneficiary and agenda setter, with constrained exit (cannot dissolve the treaty without constitutional rupture) â structurally near the beneficiary end, though its exit is not free. maori_hapu_and_iwi are declared victims with identity_locked exit (the treaty and whakapapa constitute their political identity within the state), placing them near the full-target end. The effective extraction is thus amplified for MÄori and damped for the Crown.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the arrangement as pure extraction (snare) â the Waitangi Tribunal, settlement processes, and co-governance frameworks are genuine coordination functions that benefit MÄori communities. It also prevents mislabeling as pure coordination (rope) â the Crown's retention of parliamentary override and the historical land alienation baked into the current property regime constitute asymmetric extraction that persists through the same partnership structure. The mandate has not atrophied (not a piton) because both parties actively maintain it, albeit with divergent interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translation_asymmetry_extraction,
    'Does the MÄori text control reading retroactively convert the historical land alienation mechanism into recognized theft, or does it merely overlay a coordination framework on an unchanged extraction base?',
    'Comparative analysis of land returned under treaty settlements versus land alienated under the English reading; if the net transfer remains strongly negative, the extraction base is unchanged.',
    'If unchanged, the partnership reading functions as theater over an ongoing extraction; if converted, the reading genuinely shifts the distributive baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_asymmetry_extraction, conceptual, 'Whether retrospective snare exposure changes the structural extraction base.').

omega_variable(
    consent_within_sovereignty_monopoly,
    'Can ongoing hapÅ« consent be operationalized while the Crown retains parliamentary sovereignty and the monopoly on legitimate violence?',
    'Natural experiment from jurisdictions with treaty-based shared sovereignty; structural comparison of legislative override frequency.',
    'If consent remains subordinate to statute, the partnership reading is necessarily performative and theater_ratio should be higher; if statute is systematically constrained by treaty, the reading functions as genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_within_sovereignty_monopoly, empirical, 'Operationalizability of consent under Crown sovereignty monopoly.').

omega_variable(
    foreclosure_vs_coexistence_cession_reading,
    'Does the rangatiratanga retention reading logically foreclose the crown_cession_reading within a unified legal framework, or do they persist as irreconcilable parallel readings held by different parties?',
    'Judicial behavior analysis: do courts ever simultaneously affirm MÄori text control and English cession in a single judgment, or do they choose one frame per case?',
    'If courts switch frames contextually, the kernel is distributed rather than fixed_text; if they foreclose one, the reading relation shifts to forecloses and the CS pattern changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_cession_reading, conceptual, 'Logical relationship between MÄori-text and English-text readings in judicial practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(trea_tr_t30, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(trea_tr_t60, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(trea_tr_t90, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 90, 0.6).
narrative_ontology:measurement(trea_tr_t120, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 120, 0.55).
narrative_ontology:measurement(trea_tr_t150, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 150, 0.5).
narrative_ontology:measurement(trea_tr_t180, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 180, 0.48).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(trea_be_t30, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(trea_be_t60, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(trea_be_t90, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 90, 0.65).
narrative_ontology:measurement(trea_be_t120, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 120, 0.6).
narrative_ontology:measurement(trea_be_t150, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 150, 0.58).
narrative_ontology:measurement(trea_be_t180, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 180, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(trea_su_t30, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(trea_su_t60, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(trea_su_t90, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 90, 0.7).
narrative_ontology:measurement(trea_su_t120, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement(trea_su_t150, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 150, 0.55).
narrative_ontology:measurement(trea_su_t180, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 180, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% The treaty_authority_cession kernel decomposes into at least three readings: crown_cession_reading (English text, full sovereignty), rangatiratanga_retention_reading (MÄori text, retained authority), and retrospective_snare_exposure (the extraction mechanism inherent in textual divergence). Each reading instantiates a structurally distinct constraint with independent Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
