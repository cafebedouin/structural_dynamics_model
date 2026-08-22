% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 Immutable Commandment Reading â Polygamy as Eternal Law Required for Exaltation
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the immutable_commandment_reading of the
 *   eternal_marriage_covenant kernel: Doctrine and Covenants 132 is read as
 *   an eternal, unchangeable divine law that makes plural marriage mandatory
 *   for exaltation in the highest degree of the celestial kingdom. Under this
 *   reading, the 1890 Manifesto is apostasy, federal anti-polygamy statutes
 *   create a martyrdom constraint, and no legitimate prophetic revision path
 *   exists. The reading is held by schismatic fundamentalist communities and
 *   historically by the 19th-century LDS church before the Manifesto. Federal
 *   pressure (Morrill, Edmunds, Edmunds-Tucker Acts) intensified the
 *   extraction by criminalizing compliance, forcing the community underground
 *   and deepening the theater of loyalty.
 *
 * KEY AGENTS:
 *   - ecclesiastical_authority: Primary agenda-setter (institutional/identity_locked) â administers sealing ordinance and collects institutional legitimacy from an unchangeable divine mandate.
 *   - male_practitioners: Primary legal target (moderate/trapped) â bears federal prosecution and social martyrdom costs as the price of exaltation.
 *   - women_in_plural_marriage: Structural target (powerless/trapped) â bears domestic subordination and erasure costs within plural kinship.
 *   - children_in_plural_families: Diffuse target (powerless/trapped) â bears legal illegitimacy and stigma without theological voice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.82).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.79).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 Immutable Commandment Reading â Polygamy as Eternal Law Required for Exaltation").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'fa159d39-9b4c-41f6-b832-963613d0a26a').
narrative_ontology:cs_kernel_codification('fa159d39-9b4c-41f6-b832-963613d0a26a', fixed_text).
narrative_ontology:cs_authority_grounding('fa159d39-9b4c-41f6-b832-963613d0a26a', lineage).
narrative_ontology:cs_reading_relation('fa159d39-9b4c-41f6-b832-963613d0a26a', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('fa159d39-9b4c-41f6-b832-963613d0a26a', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('fa159d39-9b4c-41f6-b832-963613d0a26a', foundational, divine_polygamy_mandatory_for_exaltation).
narrative_ontology:cs_axiom_status(divine_polygamy_mandatory_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('fa159d39-9b4c-41f6-b832-963613d0a26a', divine_polygamy_mandatory_for_exaltation, theological).
narrative_ontology:cs_axiom('fa159d39-9b4c-41f6-b832-963613d0a26a', foundational, immutable_commandment_no_prophetic_override).
narrative_ontology:cs_axiom_status(immutable_commandment_no_prophetic_override, holdable).
narrative_ontology:cs_axiom_grounding('fa159d39-9b4c-41f6-b832-963613d0a26a', immutable_commandment_no_prophetic_override, theological).
narrative_ontology:cs_reference_frame('fa159d39-9b4c-41f6-b832-963613d0a26a', eternal_celestial_law_framework).
narrative_ontology:cs_drift_state('fa159d39-9b4c-41f6-b832-963613d0a26a', post_manifesto_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('fa159d39-9b4c-41f6-b832-963613d0a26a', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, male_practitioners).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_in_plural_marriage).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, children_in_plural_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the sealing ordinance and controls access to temple rites required for exaltation. Presents the revelation as unchangeable and binds the community to it through ordinances and teaching. Cannot alter the commandment without undermining the claim to prophetic access to immutable eternal law.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, ecclesiastical_authority, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Expected to enter plural marriages to secure exaltation for themselves and their sealed wives. Bear the legal risk of federal prosecution, fines, and imprisonment under anti-polygamy statutes. Refusal or abandonment of plural marriage is treated as apostasy and loss of celestial glory.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, male_practitioners, payer,
    moderate, biographical, trapped, national).

% Enter plural marriage as a condition of exaltation, often under economic and theological pressure. Bear the emotional and domestic costs of polygynous family structure, including jealousy, resource competition, and social erasure. Exit means forfeiting sealed status and eternal family promises.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, women_in_plural_marriage, payer,
    powerless, biographical, trapped, national).

% Born into legally precarious family structures. Bear stigma, illegitimacy under federal law, and inheritance vulnerabilities. Have no theological voice in the arrangement that governs their parentage.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, children_in_plural_families, payer,
    powerless, biographical, trapped, national).

% Theologically argue that monogamy suffices for exaltation or that the revelation was situational. Excommunicated or silenced under the immutable reading; their objections are read as apostasy rather than legitimate debate.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, monogamous_dissenters, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, ecclesiastical_authority).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orders celestial kinship and ecclesiastical hierarchy by binding salvation to a specific marital practice, creating a closed community of the obedient sealed for exaltation across generations.
% TRANSFER_FUNCTION: Transfers reproductive labor, domestic submission, and legal risk from women and male practitioners to the ecclesiastical authority's ledger of sealed obedience, in exchange for promised post-mortal exaltation.
% ABSENT_VOICES: Women seeking monogamous equity, children harmed by family secrecy, and federal jurists enforcing anti-polygamy statutes are excluded from theological deliberation; their objections are read as worldliness or persecution. Monogamous dissenters are excommunicated and their arguments ruled out of order.
% DISAPPEARANCE_RATIONALE: If the immutable requirement vanished, plural marriages would lose their salvific rationale, the ecclesiastical authority's claim to unchangeable keys would fracture, and the community's boundary against the world would collapse. Federal prosecutions would lose their religious target, and family structures would reorganize around monogamy or dispersal.
% FOUNDING_PROBLEM: Restoration of all things, including Old Testament patriarchal order, and the sealing of kinship networks across generations to secure exaltation in the highest degree of the celestial kingdom.
% FOUNDING_PROBLEM_CORROBORATION: Fundamentalist dissenters and some early Utah-era diarists attest the problem as live from within the tradition. No external corroboration exists outside the benefiting theological framework; federal courts and secular historians treat the founding claim as historically constructed rather than divinely mandated.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82 at peak) because the constraint extracts legal safety, reproductive labor, and domestic autonomy under threat of eternal consequences. Suppression is high (0.79) because alternatives (monogamous exaltation, civil compliance) are theologically collapsed. Theater_ratio rises to 0.55 as federal pressure forces the community into performative loyalty and underground persistence. Accessibility_collapse is very high (0.88) because within the theological frame, rejecting plural marriage means forfeiting exaltation. Resistance is high (0.72) because federal enforcement and internal dissent actively contest the arrangement. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is the restoration of eternal celestial law â genuine coordination of exaltation. From the payer seats, it is enforced extraction of legal safety, bodily autonomy, and family structure under threat of damnation. The engine computes this divergence from the structural data; the immutable reading's claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical authority sits at the beneficiary end: it collects obedience and legitimacy without bearing legal persecution. Male practitioners and women in plural marriage sit at the target end: the constraint extracts legal risk, domestic labor, and reproductive compliance from them. Children are downstream targets bearing structural stigma. The directionality gap between the authority (low d) and the practitioners (high d) is what produces the seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving the genuine coordination function (sealing cosmology, intergenerational kinship) alongside the asymmetric extraction (patriarchal subordination, federal martyrdom). Without the coordination element, it would be a pure snare; without the extraction element, it would be a rope. The active enforcement requirement (excommunication, social boundary) and the presence of both beneficiaries and victims keep it in the tangled rope category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression driven by external federal coercion or by internalized theological fear of losing exaltation?',
    'Post-manifesto trajectory: if practice persists despite removal of federal penalties, suppression is primarily internalized; if it collapses, it was structurally dependent on state enforcement.',
    'Internalized suppression indicates higher effective extraction than structural measure suggests; reclassification toward snare or stronger tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in theological martyrdom constraint').

omega_variable(
    coordination_extraction_boundary,
    'Does the sealing cosmology constitute genuine coordination of kinship across generations, or is the coordination narrative cover for patriarchal extraction?',
    'Comparative analysis of kinship outcomes in plural vs monogamous Mormon families across generations; if plural families show no distinctive cooperative advantage, coordination is cover.',
    'If cover, extraction is higher and the rope element is theater; if genuine, coordination cost is real and extraction is partially justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether sealing cosmology is genuine coordination or cover for extraction').

omega_variable(
    kernel_immutability_status,
    'Is the immutability of D&C 132 a fixed textual property or a reading-dependent construct that serves institutional authority?',
    'Textual-historical analysis of the revelation''s production and the 1890 Manifesto''s doctrinal framing within the same textual corpus.',
    'If the text itself contains ambiguities allowing suspension, the immutable reading is a constructed constraint and the foreclosure relations weaken.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_immutability_status, conceptual, 'Whether immutability is intrinsic to the text or a constructed reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(eter_tr_t50, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(eter_be_t50, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(eter_su_t50, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
