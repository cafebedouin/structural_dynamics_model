% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Commemoration Rite — Mourning Practice Reading
 *   domain: religious studies/collective memory/ritual practice
 *
 * SUMMARY:
 *   A survivor-descendant community scattered by a catastrophe three
 *   generations back maintains an annual commemoration cycle: a fixed
 *   memorial day, a liturgy of testimony and lament led by the memorial
 *   foundation's staff, a hall of photographs, and a traveling exhibit. The
 *   rite binds dispersed households to a shared calendar and a common account
 *   of what happened, honors the dying generation of eyewitnesses, and hands
 *   the young a ready-made identity. It teaches no skills: there are no
 *   drills, no hazard curricula, no operational component — its entire output
 *   is symbolic. Participation is voluntary; households opt in and out year
 *   to year at the cost of a few inquiries. Claim and metrics are independent
 *   authored facts: the rite is CLAIMED as a coordination mechanism (identity
 *   maintenance without coercion), while the metrics describe its measured
 *   operation — low extraction that creeps upward as living memory fades and
 *   form begins to outlive felt content for some seats.
 *
 * KEY AGENTS:
 *   - - commemorating_community: Primary beneficiary (organized/mobile) — receives identity continuity, shared calendar, and belonging; bears only the costs of its own chosen participation
 *   - - memorial_officiants: Agenda setter (institutional/identity_locked) — designs and administers the rite; collects standing, salaried positions, and endowed infrastructure
 *   - - descendant_generation_youth: Dual-positioned participant (moderate/mobile) — inherits both the identity good and a soft obligation it did not originate
 *   - - aging_survivor_witnesses: Honored anchor (moderate/immediate horizon) — lends the rite its authority; receives honor and the assurance of being carried forward
 *   - - lapsed_descendants: Excluded voice (moderate/mobile) — symbolically enrolled without consent; outside the planning circle by virtue of nonattendance
 *   - - memory_studies_researchers: Analytical observer (analytical/analytical) — tests whether the rite's output is symbolic only or includes operational capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.09).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.09).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Commemoration Rite — Mourning Practice Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious studies/collective memory/ritual practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, '13d3f72c-47bc-45c1-931c-5543d92fefa1').
narrative_ontology:cs_kernel_codification('13d3f72c-47bc-45c1-931c-5543d92fefa1', distributed).
narrative_ontology:cs_authority_grounding('13d3f72c-47bc-45c1-931c-5543d92fefa1', practice).
narrative_ontology:cs_interpretation_layer_present('13d3f72c-47bc-45c1-931c-5543d92fefa1').
narrative_ontology:cs_reading_relation('13d3f72c-47bc-45c1-931c-5543d92fefa1', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('13d3f72c-47bc-45c1-931c-5543d92fefa1', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('13d3f72c-47bc-45c1-931c-5543d92fefa1', foundational, no_operational_transfer).
narrative_ontology:cs_axiom_status(no_operational_transfer, holdable).
narrative_ontology:cs_axiom_grounding('13d3f72c-47bc-45c1-931c-5543d92fefa1', no_operational_transfer, empirically_contingent).
narrative_ontology:cs_axiom('13d3f72c-47bc-45c1-931c-5543d92fefa1', secondary, consent_based_legitimacy).
narrative_ontology:cs_axiom_status(consent_based_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('13d3f72c-47bc-45c1-931c-5543d92fefa1', consent_based_legitimacy, deontological).
narrative_ontology:cs_reference_frame('13d3f72c-47bc-45c1-931c-5543d92fefa1', living_symbolic_commemoration).
narrative_ontology:cs_drift_state('13d3f72c-47bc-45c1-931c-5543d92fefa1', post_living_memory_transition, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('13d3f72c-47bc-45c1-931c-5543d92fefa1', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, memorial_officiants).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, aging_survivor_witnesses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, descendant_generation_youth).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__mourning_practice_reading, descendant_generation_youth).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, commemorative_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The body of families descended from the catastrophe's survivors who keep the annual commemoration cycle: they gather on the fixed memorial date, fund the memorial hall and its upkeep through voluntary dues and donations, and host the traveling exhibit of photographs and testimonies. What flows to them is a shared calendar, a common account of what happened, and the standing of belonging to a community that remembers together. Any household may skip a given year's observance; the usual consequence is a few inquiries and a quieter place at next year's gathering.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community, beneficiary,
    organized, generational, mobile, national).

% The salaried and volunteer staff of the memorial foundation — the cantor, the archivists, the education coordinator — who set the liturgy, fix the calendar, decide which testimonies enter the ceremony, and train the next cohort of readers. Commemorative giving funds their positions and the building they maintain. Leaving the role would mean leaving the vocation around which their working lives and reputations are organized.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, memorial_officiants, agenda_setter,
    institutional, biographical, identity_locked, regional).

% Members in their teens and twenties who were born long after the catastrophe. They receive language, history, and a ready-made answer to the question of who their people are, and they give the ceremony its future readers and singers. Many attend gladly; others come mainly because grandparents and parents expect it, giving an afternoon and an emotional performance they did not choose. Opting out is possible and occasionally exercised, at the price of difficult conversations at holiday tables.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, descendant_generation_youth, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, descendant_generation_youth, payer).

% The shrinking cohort of men and women who lived through the catastrophe and whose eyewitness accounts anchor the ceremony's central hour. The rite confers honor on them and undertakes to carry their account after they die; in return they lend the event its authority and sit, frail, through retellings of the worst years of their lives. Some find the annual retelling sustaining; some find it an imposition they accept for the sake of the young.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, aging_survivor_witnesses, beneficiary,
    moderate, immediate, mobile, regional).

% Descendants who have married out, moved away, or simply stopped coming, and who are nonetheless spoken of at the ceremony as part of the community that remembers. No one consults them on the liturgy; their absence is treated as drift rather than decision. Seated at the planning table, they would object to being enrolled symbolically in a practice they never consented to, and to commemorative money going to buildings and exhibits rather than to the living.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, lapsed_descendants, excluded,
    moderate, generational, mobile, national).

% Academic students of commemoration who attend with notebooks, compare this community's rite with memorial practices worldwide, and test whether ceremonial participation changes what members know or can do about hazards, or only what they feel and say. They collect from no one and are paid by their universities; their findings occasionally reach the foundation board, where they are usually acknowledged and filed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, memory_studies_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__mourning_practice_reading, memorial_officiants).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__mourning_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of carrying a shared identity and a shared account of a catastrophe across generations once the witnesses die: it synchronizes a dispersed population on one memorial date, supplies a common script of remembrance, marks the boundary of belonging, and gives each generation a defined occasion to hand the account to the next.
% TRANSFER_FUNCTION: Moves time, attention, emotional labor, and commemorative giving from participating households into the annual ceremony and the memorial institutions that stage it; moves honor and standing to the surviving witnesses and the officiating staff; returns identity continuity, historical narrative, and belonging to the participants.
% ABSENT_VOICES: Lapsed descendants — the married-out, moved-away, and drifted-away — are spoken of as 'the community' at a ceremony they do not attend and were never asked to authorize; their nonattendance removes them from the planning circle precisely because showing up is the price of a voice there. Internal dissenters who would redirect commemorative spending toward the living are present but marginal in foundation deliberations.
% DISAPPEARANCE_RATIONALE: The community's calendar, the memorial hall's purpose, the officiants' vocations, and the annual rhythm of homecoming all presuppose the rite. Overnight disappearance would force improvisation — ad hoc gatherings, private family storytelling — strand the foundation's staff and donor commitments, and leave the desire for continuity to rebuild some observance within a few years, almost certainly in a different form.
% FOUNDING_PROBLEM: In the founding decade the community was scattered and bereaved: survivors were dispersed across cities and countries, the dead outnumbered the living in every conversation, and children born after the catastrophe had no vehicle for learning what had happened or to whom they belonged. The rite was built to reassemble the remnants around a fixed date and a shared account.
% FOUNDING_PROBLEM_CORROBORATION: Archival community minutes from the founding decade and contemporaneous press coverage — sources outside the benefiting parties — corroborate the founding problem: the rite was designed to reassemble a scattered, bereaved population. Whether the problem remains live today is attested only by interested parties: officiants and engaged elders say each generation must be taught anew, while lapsed descendants and some youth say the need the rite answers no longer includes them; no disinterested body tracks the question.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.22 at interval end) because the rite's costs — time, emotional labor, commemorative giving — are largely the consensual price of a good participants receive, and because nothing is taken from anyone who does not show up. The slow creep across the interval tracks habit-driven participation in later generations: the same afternoon given with less conviction is a slightly less consensual expenditure. Suppression is very low (0.09) and static: there is no enforcement machinery at all — persistence is by consent — so per the static-enforcement rule no suppression_requirement series is authored; the scalar carries the whole picture. Theater stays well below 0.5 because under this reading performance IS the function: symbolic output is the declared product, so staging the rite is doing the work, not mimicking it. The theater rise from 0.08 to 0.26 tracks identifiable slippage — officials attending for optics, rote recitation among some youth, exhibit upkeep outlasting visitation — not conversion of the rite into shell. Accessibility collapse is low (0.15): substitutes abound (private mourning, new domestic rituals, secular commemoration, simple exit), and nothing blocks them. Resistance is low (0.10): individual opt-outs and occasional internal criticism of spending, never organized opposition. Coordination type is identity_coordination and the function is genuine — boundary maintenance and membership transmission are exactly what the rite does — so the modest excess above the type floor is the honest signal that some commemorative resource flow serves institutional upkeep beyond bare coordination. The measurement grid spans generations (0–75) and deliberately smooths the annual cycle; the tracked dynamic is cross-generational drift, not intra-year oscillation.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same rite differently. From the officiants' seat it is a living tradition they steward — their computed classification should sit nearest pure coordination. From the youth seat it alternates between inheritance and obligation depending on the household. From the lapsed descendants' seat it is a claim made in their name without their consent — a cost imposed from outside the arrangement's perimeter. From the researcher seat it is a symbolic-output machine whose operational null is an open empirical question. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries cluster at the subsidized end: the community receives the identity good, the witnesses receive honor and continuity assurance, and the officiants receive standing and endowed positions — all low-d seats. The youth seat is dual-positioned and derives slightly higher d than pure beneficiaries, reflecting the unchosen share of their contribution. Critically, no seat approaches the full-target end: the structural signature of this reading is the ABSENCE of a target seat. The two nearest cost-bearing phenomena — youth soft obligation and lapsed symbolic enrollment — are routed to omega variables rather than declared as victims, because under this reading the first is a consensual price whose compulsion level is an open question, and the second falls outside the arrangement's perimeter entirely. Declaring either as a victim would author a different constraint than the one this reading instantiates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reassemble the scattered, transmit the account — has thinned but not died, and the rite persists by renewed consent each year rather than by inertia. The cheap-exit test is decisive: if the community stopped valuing continuity, nothing would hold the rite up — there is no enforcement apparatus to decay into performance-only persistence, no vested coercive interest to keep a dead form alive. That is why this reading resists the atrophy conclusion: function and mandate remain aligned even as intensity fades, and a fading-but-consented rite is not a piton in waiting. The mandatrophy mislabel risk runs in the opposite direction here: reading the rite's costs as a levy would mistake the price of a good for extraction, and reading its institutional upkeep as capture would mistake the salary of the person who does the work for rent collected from someone who doesn't.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the mourning_practice_reading of the catastrophe_memory_preservation kernel. What would each sibling reading change structurally if adopted?',
    'Read the sibling files (catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading) against this one; the disagreement is located at one node — whether the rite''s causal output terminates in symbolic effect or includes operational threat-recognition capacity.',
    'Under survival_competence_reading the rite acquires operational stakes, obligations, and probable enforcement (higher epsilon, an emergent victim set, suppressed exits). Under hybrid_atrophy_reading the same rite is residue of a lost function, pushing theater_ratio toward piton territory. This file''s low-suppression, no-victim profile is valid only under the mourning reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings are separate constraints, not alternative measurements of this one.').

omega_variable(
    operational_transfer_null,
    'Does ceremonial participation in fact produce no measurable improvement in hazard knowledge, preparedness behavior, or threat recognition — the null this reading asserts?',
    'Longitudinal comparison of participants and non-participants within the community on preparedness measures and hazard knowledge, controlling for direct family transmission of survivor knowledge.',
    'A positive transfer finding collapses this reading toward survival_competence_reading: the rite would be doing training work, with the obligations, stakes, and enforcement implications that follow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_transfer_null, empirical, 'The empirical crux internal to the mourning reading: the operational-transfer null.').

omega_variable(
    soft_obligation_boundary,
    'Is youth participation genuinely opt-in, or softly compelled by family expectation to a degree that constitutes bearing imposed costs?',
    'Participation-decision surveys and opt-out accounting: who declines, at what reported relational cost, and whether decliners suffer lasting standing losses in the community.',
    'If compulsion is substantial, a victim set emerges inside the participant body and the arrangement becomes a hybrid — a real coordination function carrying asymmetric imposed costs — rather than the clean voluntary marker this reading describes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_obligation_boundary, empirical, 'Whether the no-victim-set claim survives contact with family-level expectation.').

omega_variable(
    identity_referent_scope,
    'Whose identity does the rite preserve — the participating subgroup''s, or a nominal descent-community that includes lapsed and absent members who never consented?',
    'Textual analysis of the liturgy''s enrollment language (''we'', ''our community'') against actual participation rolls, plus structured consultation of lapsed members on whether they accept symbolic enrollment.',
    'If the referent is the nominal community, the rite imposes symbolic enrollment on non-participants, raising effective extraction for lapsed seats and complicating the voluntary-marker picture; if the referent is participants only, the reading stands clean as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_referent_scope, conceptual, 'Scope of the ''collective'' in collective identity: participants or nominal community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_mourn_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cmp_mourn_tr_t0, observed).
narrative_ontology:measurement(cmp_mourn_tr_t15, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(cmp_mourn_tr_t15, observed).
narrative_ontology:measurement(cmp_mourn_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(cmp_mourn_tr_t30, observed).
narrative_ontology:measurement(cmp_mourn_tr_t45, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement_basis(cmp_mourn_tr_t45, observed).
narrative_ontology:measurement(cmp_mourn_tr_t60, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(cmp_mourn_tr_t60, observed).
narrative_ontology:measurement(cmp_mourn_tr_t75, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 75, 0.26).
narrative_ontology:measurement_basis(cmp_mourn_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(cmp_mourn_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(cmp_mourn_be_t0, observed).
narrative_ontology:measurement(cmp_mourn_be_t15, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement_basis(cmp_mourn_be_t15, observed).
narrative_ontology:measurement(cmp_mourn_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(cmp_mourn_be_t30, observed).
narrative_ontology:measurement(cmp_mourn_be_t45, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 45, 0.2).
narrative_ontology:measurement_basis(cmp_mourn_be_t45, observed).
narrative_ontology:measurement(cmp_mourn_be_t60, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 60, 0.21).
narrative_ontology:measurement_basis(cmp_mourn_be_t60, observed).
narrative_ontology:measurement(cmp_mourn_be_t75, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 75, 0.22).
narrative_ontology:measurement_basis(cmp_mourn_be_t75, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'catastrophe memory ritual' decomposes into three structurally distinct claims about what the rite preserves, per the epsilon-invariance principle. This file authors the mourning_practice_reading (symbolic continuity without operational transfer; voluntary participation; epsilon ~0.22; no victim set). The survival_competence_reading authors the same rites as intergenerational threat-recognition training (empirically contested; higher stakes; obligations and probable enforcement). The hybrid_atrophy_reading authors them as atrophied residue of a former training function (elevated theater; inertial persistence). All three share a referent — the standing commemoration arrangement — and diverge on epsilon, victim structure, and type; each is epsilon-invariant within its own reading. Edges run in both directions across the family; the survival_competence_reading is the upstream claim whose plausibility the other two discount.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
