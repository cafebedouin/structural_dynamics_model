% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Reading of Divine Legitimacy (Akhenaten's Revelation)
 *   domain: religious/political economy of belief systems
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel — the divine
 *   legitimacy substrate of late Eighteenth Dynasty Egypt — specifically the
 *   Atenist monotheistic reading under which Akhenaten declares Aten the sole
 *   legitimate deity and himself the exclusive channel of revelation. Under
 *   this reading, the traditional multi-deity cosmology administered by the
 *   Amun priesthood is not merely superseded but declared false; the
 *   constraint requires dismantling an existing temple economy (land,
 *   personnel, ritual calendar) and rebuilding a parallel court-centered cult
 *   at a new capital. The rapid, near-complete reversal after the reign's end
 *   is read, from this reading's own vantage, as evidence the claim never
 *   achieved durable coordination status — it required continuous active
 *   enforcement (erasure of rival names, confiscation of temple estates,
 *   relocation of administration) for its entire operative life and collapsed
 *   the moment that enforcement lapsed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.81).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.88).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Reading of Divine Legitimacy (Akhenaten's Revelation)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political economy of belief systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '0a3b8fec-cf94-4f35-bc12-3f2c74171d51').
narrative_ontology:cs_kernel_codification('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', formalized).
narrative_ontology:cs_authority_grounding('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', extraction).
narrative_ontology:cs_interpretation_layer_present('0a3b8fec-cf94-4f35-bc12-3f2c74171d51').
narrative_ontology:cs_reading_relation('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', divine_legitimacy_substrate__folk_syncretistic_reading, influences).
narrative_ontology:cs_axiom('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', foundational, aten_sole_true_deity).
narrative_ontology:cs_axiom_status(aten_sole_true_deity, holdable).
narrative_ontology:cs_axiom_grounding('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', aten_sole_true_deity, theological).
narrative_ontology:cs_axiom('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', foundational, pharaoh_exclusive_revelatory_channel).
narrative_ontology:cs_axiom_status(pharaoh_exclusive_revelatory_channel, holdable).
narrative_ontology:cs_axiom_grounding('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', pharaoh_exclusive_revelatory_channel, conventional).
narrative_ontology:cs_axiom('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', secondary, prior_multi_deity_cosmology_categorically_false).
narrative_ontology:cs_axiom_status(prior_multi_deity_cosmology_categorically_false, holdable).
narrative_ontology:cs_axiom_grounding('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', prior_multi_deity_cosmology_categorically_false, theological).
narrative_ontology:cs_reference_frame('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', pre_amarna_multi_temple_cosmology).
narrative_ontology:cs_drift_state('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', post_restoration_administration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('0a3b8fec-cf94-4f35-bc12-3f2c74171d51', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_royal_household).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_cult_new_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, akhetaten_court_officials).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_dependent_artisans).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, provincial_temple_estates).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, village_ritual_practitioners).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, solar_monotheism_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_as_sole_divine_intermediary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares Aten the sole legitimate deity and itself the exclusive channel of revelation, redirecting cultic authority, land revenue, and administrative appointment away from the old temple network and into a new court-centered cult based at Akhetaten. Controls the definition of legitimacy itself; nothing external checks the declaration while the reign holds.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_royal_household, agenda_setter,
    institutional, generational, analytical, national).

% Newly appointed personnel whose offices, stipends, and status exist only because the old priesthoods were displaced. Their position depends entirely on the pharaoh's continued monopoly claim; they have no independent base of legitimacy or resources to fall back on if the arrangement reverses.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_cult_new_priesthood, beneficiary,
    organized, biographical, constrained, national).

% Administrators relocated to the new capital, granted offices and land grants contingent on the new theology's supremacy. They help enforce the exclusivity claim through appointments, inscriptions, and construction projects that make the new order visible and irreversible-seeming.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, akhetaten_court_officials, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, akhetaten_court_officials, agenda_setter).

% Stripped of temple income, cultic authority, and the vast administrative and economic apparatus built around Amun-Ra worship over centuries. Names erased from monuments, personnel dispersed or absorbed elsewhere. Cannot practice publicly; their entire institutional existence depends on the theology the new order declares false.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    organized, generational, trapped, national).

% Craftsmen, scribes, and laborers whose livelihoods depended on temple commissions, festival economies, and endowed land now redirected to the Aten cult. Have no organized voice and no realistic option beyond migrating toward the new capital's construction economy or losing their livelihood entirely.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_dependent_artisans, payer,
    powerless, biographical, trapped, regional).

% Landholding institutions across the provinces that held wealth and local authority through traditional cults. Revenue confiscated or redirected; local administrative influence collapses along with the cults they served. Geographically dispersed and cannot coordinate resistance against a centralized royal decree.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, provincial_temple_estates, payer,
    moderate, generational, trapped, national).

% Ordinary households whose devotional practice toward household and local deities is declared illegitimate by the new exclusive theology, though enforcement reaches unevenly into daily village life. They have no formal voice in the court's theological declarations and continue much informal practice quietly, outside official visibility.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, village_ritual_practitioners, excluded,
    powerless, biographical, constrained, local).

% The successor administration (post-Akhenaten) that reverses the exclusivity claim, restores Amun's temples, and erases Akhetaten's monuments — providing the retrospective record by which the exclusivity period's costs and beneficiaries become visible.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, later_restoration_authorities, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_royal_household).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes religious and administrative authority under a single interpretive source (pharaoh-as-sole-revealer), in principle simplifying the fragmented, competing claims of multiple temple hierarchies into one chain of legitimacy and one unified court-controlled bureaucracy.
% TRANSFER_FUNCTION: Moves land revenue, cultic income, administrative appointments, and symbolic authority away from the Amun priesthood and provincial temple networks toward the royal household, the new Aten priesthood, and the Akhetaten court establishment.
% ABSENT_VOICES: The Amun priesthood and provincial temple administrators are stripped of platform and voice entirely — their names are removed from monuments. Village-level practitioners maintaining folk devotional practice have no standing in the court's theological pronouncements and are simply absent from the record of legitimate discourse.
% DISAPPEARANCE_RATIONALE: The exclusivity claim's disappearance (which historically occurred within a generation) triggered immediate restoration: Amun's temples reopened, confiscated lands and personnel reverted, the new capital was abandoned and its monuments defaced. The rapid, near-total reversal demonstrates the arrangement depended entirely on active royal enforcement rather than any self-sustaining coordination logic.
% FOUNDING_PROBLEM: Framed by the pharaonic court as solving theological confusion and fragmented cultic authority by revealing the singular true source of divine legitimacy; also functioned to break the accumulated economic and political power the Amun priesthood had concentrated relative to the throne.
% FOUNDING_PROBLEM_CORROBORATION: No corroboration exists from outside the pharaonic court and its appointed cult — the restoration administration that followed, along with the erasure of Akhetaten's name from later king-lists, constitutes the strongest available evidence that the founding claim was not accepted as solving any problem the wider religious and administrative establishment recognized as real. The absence of any surviving non-court voice affirming the revelation is itself the corroboration record.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises sharply and early (0.35 to 0.81 across the interval) because the exclusivity claim required an accelerating transfer of temple wealth and administrative authority to the new court cult in a compressed period rather than the constraint settling into low-cost steady-state coordination. Suppression rises even faster and higher (0.40 to 0.88) because maintaining a monotheistic exclusivity claim against an entrenched, wealthy, geographically dispersed polytheistic establishment required continuous coercive effort — erasure campaigns, confiscation, and enforced relocation — that a genuinely settled coordination arrangement would not need. Theater ratio climbs moderately (0.20 to 0.42): monumental construction and iconographic campaigns performed the new theology's inevitability, but a substantial share of activity (temple confiscation, appointment reallocation) was not merely performative — it materially redirected resources, so theater does not dominate the metric profile the way it would in a pure piton.
 *
 * PERSPECTIVAL GAP:
 *   From the pharaonic court's seat, the arrangement instantiates a coordination improvement: one interpretive source instead of competing priestly claims, one unified administrative hierarchy instead of fragmented temple estates. From the Amun priesthood's seat, the identical structure is straightforward asymmetric extraction backed by coercion — their wealth and authority did not evaporate through obsolescence but through confiscation enforced by the same apparatus that declared their theology false. The engine's tangled_rope computation should reflect this: a genuine (if contested) coordination claim coexisting with unmistakable, enforcement-dependent asymmetric extraction from a named victim class.
 *
 * DIRECTIONALITY LOGIC:
 *   The pharaonic household and the new Aten priesthood sit at the beneficiary end: they collect the redirected land revenue, administrative appointments, and symbolic centralization the exclusivity claim produces, and their institutional existence is definitionally tied to the claim's success. The Amun priesthood and provincial temple estates sit at the target end: organized but geographically dispersed, unable to coordinate resistance against a centralized royal decree, and stripped of both income and public legitimacy. Village-level practitioners are excluded rather than directly targeted — the claim reaches them unevenly, and their informal continuation of prior practice below official visibility is itself evidence of accessibility collapse being incomplete rather than total.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (theological confusion, fragmented cultic authority) is read by this reading's proponents as live and solved by centralization; but the founding_problem_status is authored dead because the rapid, comprehensive restoration under the successor administration — with no surviving non-court corroboration of the original claim's benefit — indicates the arrangement was never accepted as solving a problem recognized outside the benefiting court faction. This prevents the constraint from being mislabeled a durable Mountain-like natural religious evolution: it was a court-imposed, enforcement-dependent reallocation whose entire operative life required active suppression, and whose disappearance produced immediate, near-total world-rearrangement (temple restoration, capital abandonment, name erasure) rather than a quiet continuation — the signature of extraction rather than settled coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_revelation_vs_political_instrument,
    'Was the Atenist exclusivity claim a genuine theological conviction on Akhenaten''s part that happened to have politically convenient effects, or was it primarily instrumentalized from the outset to break Amun priesthood''s accumulated economic and political power relative to the throne?',
    'Comparative analysis of the timing and sequencing of theological pronouncements against land confiscation records and administrative appointment records; if confiscation preceded or closely tracked theological escalation rather than following organic religious conviction, the instrumental reading gains support.',
    'If primarily instrumental, this strengthens the tangled_rope classification (extraction with a thin coordination cover); if genuinely doctrinal with extraction as an unintended byproduct, the coordination function claim gains more independent weight, though the victim-facing metrics would likely remain unchanged either way.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_revelation_vs_political_instrument, conceptual, 'Whether the exclusivity claim''s origin was primarily religious conviction or political instrument.').

omega_variable(
    committer_framing_alternative_kernel_reading,
    'Is the divine_legitimacy_substrate kernel best modeled with the Atenist claim as one reading among three coexisting positions, or does the Atenist claim''s totalizing exclusivity structurally foreclose the possibility of the other two readings persisting within the same political framework (since it declares them false rather than merely alternative)?',
    'Examine whether the Amun and folk-syncretistic readings continued to operate in practice (even suppressed/informal) during the Atenist period, versus whether the exclusivity claim achieved actual totalizing displacement during its operative window.',
    'If the sibling practices persisted informally throughout (as village-level evidence suggests), coexists_with is the correct relation; if the court''s enforcement achieved genuine — if temporary — totalizing displacement of practice, a stronger forecloses relation to at least the amun_polytheistic_reading within the formal administrative framework (though not within lived practice) would be defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_alternative_kernel_reading, conceptual, 'Whether the Atenist reading forecloses or merely coexists with sibling readings within the same political framework during its operative period.').

omega_variable(
    temple_economy_scale_uncertainty,
    'What fraction of pre-Amarna Egypt''s total administered land and economic activity actually flowed through Amun temple estates, and thus how large was the transfer this constraint effected?',
    'Archaeological and papyrological estimates of temple landholding as a fraction of total arable/administered land in the late Eighteenth Dynasty, cross-referenced with post-restoration inventories.',
    'A larger estimated temple economy share would support a higher extractiveness score and a more severe reading of the transfer function; a smaller share would moderate the magnitude claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_economy_scale_uncertainty, empirical, 'Uncertainty in the economic scale of the temple estates affected by the transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(divi_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.76).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 15, 0.81).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.83).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.87).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.06).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the divine_legitimacy_substrate kernel. amun_polytheistic_reading and folk_syncretistic_reading are separate constraint files, each with independently authored ε, beneficiaries, victims, and stakeholder surfaces reflecting their own structural claims about where legitimacy flows from. This file's forecloses relation to amun_polytheistic_reading reflects that the Atenist exclusivity claim's core premise (all other gods are false) directly and categorically contradicts the Amun reading's core premise (Amun-Ra as legitimate chief patron among a multi-deity cosmology) within any single formal administrative framework, even though the folk_syncretistic practice appears to have persisted informally underneath both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
