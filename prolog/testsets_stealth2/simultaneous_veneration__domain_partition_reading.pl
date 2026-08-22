% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Dual-Track Domain Partition of Kami and Buddha Veneration (Domain-Partition Reading)
 *   domain: religious/comparative-religion/japanese-history
 *
 * SUMMARY:
 *   From roughly the eighth century, as buddhist temples consolidated across
 *   the archipelago alongside the older kami shrines, a customary division of
 *   ritual labor stabilized: shrines handled this-worldly concerns — rain,
 *   harvest, health, protection, purification — while temples handled death —
 *   funerals, memorials, rites for favorable rebirth. Households and elite
 *   patrons routed needs accordingly; both cults flourished without
 *   suppressing the other, and participation was renewed voluntarily for
 *   centuries. This file instantiates ONE reading of the contested
 *   simultaneous_veneration kernel — the domain_partition_reading — as a
 *   clean, epsilon-invariant constraint: two parallel provider tracks with
 *   independent extraction profiles and no victim set. The sibling readings
 *   (ontological fusion; pragmatic incoherence) are separate constraint files
 *   in the same family, linked via network.affects_constraints; their epsilon
 *   values and structures are authored there, not hedged or averaged here.
 *   The claim and the metrics are independent authored facts: the rope claim
 *   is asserted from the authoring seat, and the metrics below describe the
 *   arrangement's actual operation as the record shows it.
 *
 * KEY AGENTS:
 *   - - shrine_priesthoods: Life-domain service provider (organized/constrained) — conducts this-worldly rites, receives matching patronage
 *   - - temple_clergy: Death-domain service provider (organized/constrained) — conducts funerary and salvific rites, receives matching patronage
 *   - - devotee_households: Primary participant-payer (moderate/constrained) — routes needs by domain, pays offerings and fees, nets a surplus of received services over paid costs
 *   - - patron_aristocratic_lineages: Elite allocator (powerful/mobile) — endows both camps by purpose and can redirect gifts at will
 *   - - court_ritual_establishment: Administrator-ratifier (institutional/arbitrage) — maintains the rite calendar and registries, draws stipend and prestige
 *   - - unaffiliated_ascetics: Excluded extra-institutional practitioner (powerless/mobile) — serves both domains without charter, marginalized by the division
 *   - - comparative_religion_scholars: Analytical observer (analytical/analytical) — sees the whole division-of-labor structure across seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.12).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.04).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Dual-Track Domain Partition of Kami and Buddha Veneration (Domain-Partition Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/comparative-religion/japanese-history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, 'a0d4ef1d-7362-4497-aaeb-201368908c5d').
narrative_ontology:cs_kernel_codification('a0d4ef1d-7362-4497-aaeb-201368908c5d', distributed).
narrative_ontology:cs_authority_grounding('a0d4ef1d-7362-4497-aaeb-201368908c5d', practice).
narrative_ontology:cs_interpretation_layer_present('a0d4ef1d-7362-4497-aaeb-201368908c5d').
narrative_ontology:cs_reading_relation('a0d4ef1d-7362-4497-aaeb-201368908c5d', simultaneous_veneration__ontological_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('a0d4ef1d-7362-4497-aaeb-201368908c5d', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('a0d4ef1d-7362-4497-aaeb-201368908c5d', foundational, kami_buddha_entity_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_entity_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('a0d4ef1d-7362-4497-aaeb-201368908c5d', kami_buddha_entity_distinctness, theological).
narrative_ontology:cs_axiom('a0d4ef1d-7362-4497-aaeb-201368908c5d', secondary, domain_appropriate_veneration_routing).
narrative_ontology:cs_axiom_status(domain_appropriate_veneration_routing, holdable).
narrative_ontology:cs_axiom_grounding('a0d4ef1d-7362-4497-aaeb-201368908c5d', domain_appropriate_veneration_routing, instrumental).
narrative_ontology:cs_reference_frame('a0d4ef1d-7362-4497-aaeb-201368908c5d', dual_track_domain_specialism).
narrative_ontology:cs_drift_state('a0d4ef1d-7362-4497-aaeb-201368908c5d', meiji_shinbutsu_bunri_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a0d4ef1d-7362-4497-aaeb-201368908c5d', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, temple_clergy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, devotee_households).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, patron_aristocratic_lineages).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, court_ritual_establishment).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, genze_riyaku_doctrine).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, domain_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary priestly lineages serving shrines across the provinces. They conduct rites for rain, harvest, health, childbirth, protection, and purification, and receive offerings, rice land, and festival labor earmarked for these life-concerns. Their clientele depends on households continuing to bring worldly troubles to shrines rather than elsewhere; leaving the office would mean abandoning hereditary rank and the community standing tied to the shrine.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shrine_priesthoods, beneficiary,
    organized, generational, constrained, regional).

% Ordained monks and their patron networks running temples. They perform funerals, memorial services, sutra dedication for the dead, and rites aimed at favorable rebirth, supported by funerary fees, memorial subscriptions, and land endowments. Death-related needs come to them rather than to shrines; abandoning ordination would forfeit monastic standing and the learned role grieving families rely on.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, temple_clergy, beneficiary,
    organized, generational, constrained, regional).

% Farming and townsman households. They bring illnesses, crop worries, and misfortunes to shrines, and commission temples when a parent dies or ancestors need memorial care, paying offerings and fees scaled to the service. Participation is renewed season by season; moving away or switching allegiance is possible but costly in village ties.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, devotee_households, beneficiary,
    moderate, biographical, constrained, local).

% Courtier and warrior houses endowing both shrines and temples. They direct gifts by purpose — rainmaking and protective rites to shrines, rebirth and memorial rites to temples — and can redirect endowments between institutions at will, which keeps both provider camps attentive to their purposes.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, patron_aristocratic_lineages, beneficiary,
    powerful, generational, mobile, national).

% The bureaus and noble houses that maintain the official rite calendar, register shrines, and charter temples. They record which institution handles which occasion, adjudicate the rare jurisdictional dispute, and draw stipends and prestige from administering an orderly ritual economy. They rarely compel participation; mostly they ratify what custom already does.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, court_ritual_establishment, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, court_ritual_establishment, beneficiary).

% Itinerant holy men and women — mountain wanderers and charismatics outside chartered institutions — who offer healing, exorcism, and funerary help across both domains without shrine or temple charter. They would say that need does not sort into institutional boxes; they sit outside the rite registries, depend on alms, and their complaints reach no council that administers the division.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, unaffiliated_ascetics, excluded,
    powerless, immediate, mobile, regional).

% Historians of religion studying the arrangement's record — rite calendars, courtier diaries, testamentary bequests — from outside any participating institution. They can see the whole division-of-labor structure, its costs and its benefits, across every seat at once.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(simultaneous_veneration__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Routes devotional needs to the institution equipped for them: this-worldly concerns (rain, harvest, health, protection, purification) go to kami shrines; death, funerary, and salvific concerns go to buddhist temples. Solves the household-level problem of deciding where to bring which need and prevents the two cults from bidding against each other for the same petitions.
% TRANSFER_FUNCTION: Moves offerings, festival labor, and endowment patronage from devotee households and elite lineages to shrine priesthoods (for life-domain rites) and temple clergy (for death-domain rites), in exchange for ritual services matched to the petitioner's need.
% ABSENT_VOICES: Unaffiliated ascetics stand outside the division and would object that spiritual needs do not sort cleanly into two chartered boxes; they appear in neither cult's councils nor in the court offices that administer the rite calendar. Ordinary households had practical voice through participation but no formal seat where the division's terms were recorded.
% DISAPPEARANCE_RATIONALE: If the division vanished overnight, households would lose the shared map of where to bring which need; shrines and temples would begin soliciting across each other's domains (temples marketing harvest rites, shrines taking up memorial services); elite patrons would face ambiguous allocation decisions; and ritual pricing would turn opaque, since jurisdictional clarity was the source of predictable fees. The practice economy would reorganize around ad hoc negotiation between providers and petitioners.
% FOUNDING_PROBLEM: As buddhist institutions spread through the archipelago (6th-9th centuries) alongside the older kami shrines, households faced two parallel providers of ritual services with overlapping claims. Without a division, every need triggered competition between shrine and temple, duplicate payments, and confusion about which beings handled which concerns.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting clergy: court ritual compilations registering rite assignments to named institutions, courtier and monk diaries documenting household practice at need-events, and modern scholarship on this-worldly benefit seeking all attest that the sorting problem was real and that the division addressed it. The unaffiliated ascetic milieu dissented, regarding the chartered division as self-serving; that dissent is recorded but does not negate the documented coordination function.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.12 — barely above zero and below the resource-allocation coordination floor (0.15), meaning the entire measured cost is attributable to inherent coordination overhead: duplicate institutional upkeep across two provider camps and occasional double payments at domain boundaries. Suppression is 0.04 because the division carried no enforcement machinery: petitioning both cults, petitioning neither, or hiring an unchartered ascetic all remained open options; the scalar is authored as a raw structural property and is deliberately left unscaled. Theater is 0.14 because the rites ARE the service delivered — ceremonial elaboration grew over the interval but stayed load-bearing. Accessibility_collapse is 0.25: knowing the domain map channels choices rather than closing them, and workable alternatives outside the map persisted throughout. Resistance is 0.12: grumbling at boundary fees, ascetic resentment at exclusion, and sporadic jurisdictional squabbles, but no sustained opposition because net benefit was broad. The temporal series run on one shared grid (points 0-600 step 100) with both tracked metrics authored at every point; suppression_requirement is intentionally not serialized because the enforcement picture is static — the scalar covers it. Receipt surface: gain_flow is authored as 'diffuse' after checking every named seat — clergy receipts are service compensation matched to delivered rites, and the residual over-service component disperses across both clergy seats and households without concentrating anywhere. fixing_cost is 'prohibitive': removal would require re-coordinating millions of households and two entrenched provider camps against a norm each participant individually prefers keeping, against a benefit of removal near nil. That cell combination is the signature of a load-bearing rope, not of an unfixed piton — the low theater ratio, rising-not-atrophied function, and live founding problem disambiguate it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from the same structure. Devotee households experience the division as convenience: a legible map that saves them from adjudicating competing clerical claims themselves, at a price they repeatedly and voluntarily renew. The two clergy seats experience it as livelihood security: each camp's ritual economy is protected from cross-domain competition without any act of suppression. The court establishment experiences it as order: a registrable, auditable ritual calendar that yields stipend and prestige. The unaffiliated ascetics experience the same structure as exclusion: their cross-domain services undercut by an institutional map they had no part in drawing. The engine computes these divergent per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Five actors are declared beneficiaries and no actor is declared a victim, so the derivation places every paying seat near the beneficiary end of the directionality range: households and patrons pay the transfers but net a durable surplus of received services over paid costs — six centuries of voluntary renewal is the behavioral proof of positive net position. The two clergy seats collect service-matched revenue with constrained exit, placing them firmly at the subsidized end. The court establishment, declared beneficiary only secondarily, sits near symmetric: it gains legitimacy and stipend but bears administrative effort. No seat approaches the full-target end, which is exactly what the near-floor epsilon encodes. Regional and national scopes apply only modest amplification to an already-negligible base.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sorting religious needs across plural institutional providers — remains live at interval end, so no mandatrophy is declared and none should be inferred: the arrangement's function has not atrophied, its theater ratio is low and rising only slowly, and nothing about its maintenance is performative. The classification guards against two opposite misreadings. Reading the clergy's revenue streams as extraction mistakes service compensation for rent and would falsely convert a rope into a snare; the victim-free structure and voluntary renewal forbid that. Conversely, the mild upward drift in both series (epsilon 0.06 to 0.12, theater 0.05 to 0.14) is early-stage Goodhart creep — ceremonial elaboration and boundary-fee growth — worth monitoring but far from proxy replacement. The enforced parishioner-tithing systems of the early-modern era, where extraction genuinely accumulated, constitute a different arrangement outside this story's referent and interval; importing them backward would contaminate the epsilon this reading owns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_provenance,
    'This story instantiates the domain_partition_reading of the simultaneous_veneration kernel; are the authored structural facts (two provider seats, no victim set, near-floor epsilon) properties of this reading specifically rather than of the kernel as a whole?',
    'Compile all three reading-files of the kernel and compare per-seat classifications and epsilon values; convergence indicates kernel-level structure, while divergence locates the disagreement to specific structural elements.',
    'Sharp divergence across readings confirms the kernel label covers multiple distinct constraints and validates the family decomposition; convergence would suggest the three readings are stylistic variants of one constraint and should be merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Committer-frame provenance: this file is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    fusion_reading_structural_delta,
    'What would change structurally if the ontological_fusion sibling reading were adopted — that kami and buddhas are one reality under two cultural lenses — collapsing the two provider seats into a single cult with one merged epsilon?',
    'Doctrinal and documentary analysis of whether medieval sources treat entity-distinctness or ontological identity as the operative premise in routing decisions — rite assignments, bequest patterns, jurisdictional settlements.',
    'Under the fusion reading this story''s two-seat structure and independent-epsilon delta dissolve into a single constraint; the disagreement between the readings is located precisely in the entity-distinctness premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fusion_reading_structural_delta, conceptual, 'Sibling delta: ontological fusion would collapse the dual-seat structure this reading depends on.').

omega_variable(
    incoherence_reading_suppression_delta,
    'Was the domain map operative lay belief guiding action, or a retrospective gloss laid over contradictory beliefs held simultaneously without resolution, sustained only by the absence of enforcement pressure?',
    'Lay decision evidence at need-events — diaries, testaments, votive records showing whether households reasoned by domain when choosing where to bring a need, or petitioned indiscriminately.',
    'If the map is gloss, the arrangement''s persistence rested on missing enforcement rather than coordination value; suppression and theater are understated in this file and the computed type would drift toward enforced-extraction categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_reading_suppression_delta, empirical, 'Whether the coordination story reflects operative belief or post-hoc rationalization.').

omega_variable(
    boundary_case_double_payment,
    'How much of the arrangement''s cost fell on households at domain boundaries — illness, disaster, death following misfortune — where both cults could plausibly claim the case?',
    'Comparison of offering and fee records at boundary events versus single-domain events; household expenditure patterns across case types.',
    'Systematic double-billing at boundaries would raise effective epsilon above the authored near-floor value and give households a partial payer position that the current beneficiary-only declaration obscures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_case_double_payment, empirical, 'Possible hidden extraction at jurisdictional boundaries between the two tracks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(svp_domain_partition_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(svp_domain_partition_tr_t100, simultaneous_veneration__domain_partition_reading, theater_ratio, 100, 0.06).
narrative_ontology:measurement(svp_domain_partition_tr_t200, simultaneous_veneration__domain_partition_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement(svp_domain_partition_tr_t300, simultaneous_veneration__domain_partition_reading, theater_ratio, 300, 0.09).
narrative_ontology:measurement(svp_domain_partition_tr_t400, simultaneous_veneration__domain_partition_reading, theater_ratio, 400, 0.11).
narrative_ontology:measurement(svp_domain_partition_tr_t500, simultaneous_veneration__domain_partition_reading, theater_ratio, 500, 0.13).
narrative_ontology:measurement(svp_domain_partition_tr_t600, simultaneous_veneration__domain_partition_reading, theater_ratio, 600, 0.14).

% Extraction over time
narrative_ontology:measurement(svp_domain_partition_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(svp_domain_partition_be_t100, simultaneous_veneration__domain_partition_reading, base_extractiveness, 100, 0.07).
narrative_ontology:measurement(svp_domain_partition_be_t200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(svp_domain_partition_be_t300, simultaneous_veneration__domain_partition_reading, base_extractiveness, 300, 0.09).
narrative_ontology:measurement(svp_domain_partition_be_t400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 400, 0.1).
narrative_ontology:measurement(svp_domain_partition_be_t500, simultaneous_veneration__domain_partition_reading, base_extractiveness, 500, 0.11).
narrative_ontology:measurement(svp_domain_partition_be_t600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 600, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'simultaneous veneration of kami and buddhas' decomposes, per the epsilon-invariance principle, into a three-member constraint family: this file (domain_partition_reading — functionally distinct entities, domain-appropriate specialization, two parallel tracks with independent epsilon values and no victim set), the ontological_fusion_reading file (one reality under two lenses, single merged cult structure), and the pragmatic_incoherence_reading file (unresolved contradiction sustained by absent enforcement). The readings share a referent era and disagree on entity-distinctness, coherence of belief, and enforcement dependence; each file authors its own epsilon over the standing arrangement by its own lights. Internally, this reading further decomposes into a life-domain track and a death-domain track whose extraction profiles are assessed independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
