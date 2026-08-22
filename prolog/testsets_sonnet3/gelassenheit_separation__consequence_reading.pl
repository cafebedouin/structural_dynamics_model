% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Ordnung Technology Rulings by Effect on Visiting, Mutual Aid, and Rootedness
 *   domain: religious/technology governance
 *
 * SUMMARY:
 *   This story instantiates the CONSEQUENCE READING of the Gelassenheit
 *   separation kernel: Ordnung rulings on technology are made by asking what
 *   a device's placement and use will do to specific, observable community
 *   practices — visiting patterns, mutual aid labor, and geographic
 *   rootedness — rather than by asking whether the device visibly resembles
 *   worldly artifacts (the artifact reading) or whether its use functionally
 *   entangles the household in outside systems (the principle reading). This
 *   produces the characteristic fine-grained, seemingly inconsistent rulings
 *   outsiders find puzzling: a telephone permitted in a shed at the end of
 *   the farm lane (preserves visiting — you still have to walk over and use
 *   it, and it doesn't ring inside the home interrupting family time) but
 *   forbidden inside the house (would erode visiting by making remote contact
 *   substitute for a personal call); a tractor permitted for stationary
 *   belt-power work (threshing, sawing) but not for tillage (would let a
 *   family farm more land alone, undercutting the labor-sharing that binds
 *   neighbors together for planting and harvest). Extraction is low because
 *   the ruling body itself lives under the same restrictions it imposes and
 *   captures no material rent; the constraint's main cost falls on those who
 *   want technologies the consequence test disallows for reasons that feel,
 *   to them, arbitrary relative to the device's actual worldliness.
 *
 * KEY AGENTS:
 *   - ministers_and_bishops: agenda_setter (organized/identity_locked) — administer case-by-case rulings, bound by same commitment
 *   - church_district_members: beneficiary/payer (moderate/identity_locked) — keep relational patterns, absorb inconvenience
 *   - aging_and_disabled_community_members: beneficiary (powerless/trapped) — most dependent on protected mutual aid
 *   - technologically_ambitious_youth: payer (powerless/constrained) — bear foregone opportunity cost
 *   - off_farm_wage_earners: payer (moderate/constrained) — face tighter restriction under consequence test than principle test would give them
 *   - adjacent_english_neighbors: excluded (moderate/mobile) — shaped by rulings without voice
 *   - denominational_historians_and_sociologists: observer (analytical) — document cross-community variation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.14).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.28).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Ordnung Technology Rulings by Effect on Visiting, Mutual Aid, and Rootedness").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/technology governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, 'b2fe7a5e-47e5-4abd-9576-690044d67df1').
narrative_ontology:cs_kernel_codification('b2fe7a5e-47e5-4abd-9576-690044d67df1', distributed).
narrative_ontology:cs_authority_grounding('b2fe7a5e-47e5-4abd-9576-690044d67df1', practice).
narrative_ontology:cs_interpretation_layer_present('b2fe7a5e-47e5-4abd-9576-690044d67df1').
narrative_ontology:cs_reading_relation('b2fe7a5e-47e5-4abd-9576-690044d67df1', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2fe7a5e-47e5-4abd-9576-690044d67df1', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_axiom('b2fe7a5e-47e5-4abd-9576-690044d67df1', foundational, separation_is_measured_by_relational_outcome).
narrative_ontology:cs_axiom_status(separation_is_measured_by_relational_outcome, holdable).
narrative_ontology:cs_axiom_grounding('b2fe7a5e-47e5-4abd-9576-690044d67df1', separation_is_measured_by_relational_outcome, instrumental).
narrative_ontology:cs_axiom('b2fe7a5e-47e5-4abd-9576-690044d67df1', secondary, identical_devices_may_be_ruled_differently_by_context).
narrative_ontology:cs_axiom_status(identical_devices_may_be_ruled_differently_by_context, holdable).
narrative_ontology:cs_axiom_grounding('b2fe7a5e-47e5-4abd-9576-690044d67df1', identical_devices_may_be_ruled_differently_by_context, instrumental).
narrative_ontology:cs_reference_frame('b2fe7a5e-47e5-4abd-9576-690044d67df1', gelassenheit_relational_preservation).
narrative_ontology:cs_drift_state('b2fe7a5e-47e5-4abd-9576-690044d67df1', post_smartphone_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b2fe7a5e-47e5-4abd-9576-690044d67df1', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, church_district_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, extended_kin_networks).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, aging_and_disabled_community_members).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, technologically_ambitious_youth).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, off_farm_wage_earners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, church_district_members).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, gelassenheit_yieldedness_doctrine).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, community_over_individual_convenience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deliberate case-by-case Ordnung rulings on specific devices (a telephone in a shed versus a house, a tractor used only for belt-power versus field work), asking not 'is this worldly-looking' but 'what does this do to who visits whom, who helps whom, and whether families stay near each other.' They administer the ruling and can revise it at members' meetings, but are themselves bound by the same underlying commitment and cannot exit the framework without leaving the church.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, ministers_and_bishops, agenda_setter,
    organized, generational, identity_locked, local).

% Live inside the resulting rulings: they get to keep the practices that a phone in the house or a car in the driveway would erode — dropping in unannounced, working bees, multigenerational households near each other. They also absorb the inconvenience of a barn phone instead of a pocket phone, or a horse-drawn buggy for local trips, as the direct cost of preserving those patterns.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, church_district_members, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, church_district_members, payer).

% Depend most heavily on the visiting and mutual-aid patterns the ruling protects — someone checking in, a barn-raising crew, a ride to church. They have no independent means to replace these functions if community practice erodes, so the consequence-based test of the ruling operates directly on their welfare.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, aging_and_disabled_community_members, beneficiary,
    powerless, generational, trapped, local).

% Rely on geographic rootedness — farms staying in the family, children settling nearby — which the rulings actively protect by discouraging technologies (private cars, in-home phones) that would let members disperse for work or convenience.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, extended_kin_networks, beneficiary,
    moderate, generational, constrained, regional).

% Want access to devices useful for study, business, or trades that the consequence test disallows in the home even though the device itself is not visibly 'worldly.' They bear the ruling's cost as foregone opportunity and must either accept the limits, negotiate within the Ordnung, or leave the community (Rumspringa/formal exit) to gain the access.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, technologically_ambitious_youth, payer,
    powerless, biographical, constrained, local).

% Work in construction, small manufacturing, or contracting for English employers and need phones, calculators, or transportation that the consequence test restricts more tightly than a functional-isolation test would, because home use of such devices is judged by its effect on household visiting patterns, not by whether the wage work itself entangles them with outside systems.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, off_farm_wage_earners, payer,
    moderate, biographical, constrained, regional).

% Interact with the community through commerce, shared roads, and emergency services but have no voice in the Ordnung deliberations, even though the rulings on phones-in-barns and horse traffic shape the practical texture of that interaction.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, adjacent_english_neighbors, excluded,
    moderate, immediate, mobile, local).

% Study how different Ordnung traditions vary in their reasoning — some communities banning a device outright because it looks worldly, others permitting the identical device in one location and forbidding it in another based on its observed effect on visiting and mutual aid. They document the reasoning patterns without living under them.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, denominational_historians_and_sociologists, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, revisable test for adopting or rejecting technology: does this device, in this specific placement and use, strengthen or weaken face-to-face visiting, mutual aid labor, and staying near kin — rather than judging by appearance or by abstract entanglement with outside systems.
% TRANSFER_FUNCTION: Moves convenience and individual technological latitude away from members (especially the young and the wage-employed) and toward the maintenance of dense local relationships that primarily benefit the elderly, the disabled, and multigenerational kin groups who depend on those relationships continuing.
% ABSENT_VOICES: Adjacent English neighbors and off-community employers have no standing in Ordnung deliberations even though rulings on transportation and communication shape commerce and cooperation with them. Youth considering their long-term vocational options are heard informally through ministers but do not sit in the members' meeting that finalizes rulings.
% DISAPPEARANCE_RATIONALE: If the consequence-based Ordnung test disappeared and each household adopted technology purely on private preference, the specific pattern of dropping in unannounced, communal labor exchange (barn raisings, harvest crews), and children settling near parents would erode within a generation — not instantly, but the deliberate friction currently protecting these patterns would be gone, and the community's distinctive relational density would gradually resemble that of its rural non-Amish neighbors.
% FOUNDING_PROBLEM: Early Anabaptist and later Amish communities faced continuous pressure from surrounding technological change (rail, telephone, automobile, electricity, internet) that, if adopted unreflectively, tended to draw individual members outward — toward wage labor, distant markets, isolated households — eroding the face-to-face community structure the tradition holds is essential to living out mutual aid and humility (Gelassenheit).
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of religion (e.g., studies of Old Order Amish technology adoption by outside researchers such as Donald Kraybill and colleagues, who are not church members) corroborate that the practical effect of the fine-grained rulings is measurably to slow geographic dispersal and preserve mutual-aid labor patterns relative to comparable rural populations that adopted the same technologies unrestricted; this is an assessment from outside the beneficiary group, not merely the ministers' own account.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.14, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.14 at interval end) and rises only slightly because the consequence test, by design, targets narrow functional effects rather than broad prohibition — it permits more than the artifact reading would (the barn telephone) and restricts differently than the principle reading would (home telephone use judged by relational effect, not entanglement). Suppression is moderate (0.28) because enforcement is real (Ordnung violations carry social and sometimes formal consequences up to Meidung) but the rules themselves are narrowly targeted, reducing the total footprint of what must be suppressed. Theater ratio is very low (0.10) because the case-by-case reasoning is substantive deliberation, not performative compliance theater — communities visibly revise rulings as circumstances change (e.g., permitting cell phones for business use in some districts after observing effects), which is the opposite of theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (church members generally, and especially the elderly, disabled, and kin-networked) get low d because the entire apparatus exists to protect the relational goods they depend on and receive without running the deliberation themselves. Ministers sit close to symmetric — they administer the constraint but are bound by it identically to everyone else, which is why they carry identity_locked exit rather than mobile. Payers (ambitious youth, off-farm wage earners) get high d because the specific consequence-based cuts fall disproportionately on their aspirations without proportionate say in the ruling process, though their exit is only 'constrained,' not 'trapped' — formal exit via leaving the church remains available at high personal cost, distinguishing this from a pure snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (technological change eroding face-to-face community structure) remains empirically live per outside sociological corroboration, which blocks a mandatrophy verdict — this is not a dead mandate being defended by inertia. The consequence reading is specifically the version of the kernel most resistant to ossifying into pure theater, because its test is inherently adaptive: as new technologies emerge, the community must re-ask 'what does this do to visiting and rootedness' rather than checking a fixed artifact list, which requires continuous live judgment rather than rote application of an old rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consequence_test_gameability,
    'Can the consequence test be manipulated by ministers to reach a predetermined ruling by selectively emphasizing which relational effect (visiting vs. mutual aid vs. rootedness) is decisive in a given case?',
    'Compare rulings across districts using the consequence reading for structurally identical devices; if outcomes diverge based on minister discretion rather than documented relational effect, the test is being used as post-hoc justification rather than genuine evaluation.',
    'If gameable, the low measured extraction may understate true extraction, since a discretionary test dressed as empirical consequence-assessment can still serve agenda-setter preferences while appearing principled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consequence_test_gameability, empirical, 'Whether the consequence test''s apparent objectivity masks discretionary ministerial judgment.').

omega_variable(
    which_reading_is_the_true_kernel,
    'Is the consequence reading the historically prior, more authentic instantiation of Gelassenheit separation, or is it a later rationalization layered onto what was originally an artifact-based or principle-based practice?',
    'Historical-documentary research into early Ordnung records and ministerial correspondence to trace whether consequence-based reasoning appears in founding-era documents or emerges later as communities faced technologies the artifact test handled poorly.',
    'If consequence reasoning is a later development, this reading''s claim to represent the kernel''s original commitment is weaker, though this would not by itself change the reading''s present-day structural properties or classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_true_kernel, conceptual, 'Whether the consequence reading is original or a later adaptive layer on the separation kernel.').

omega_variable(
    youth_exit_as_validation_or_indictment,
    'Does the rate of youth formally leaving the church (rather than joining after Rumspringa) validate the consequence reading''s costs as acceptable to most, or indict it as extracting more from technologically ambitious youth than the tradition''s own retention data can bear?',
    'Track baptism/retention rates across districts using consequence-reading Ordnungs versus artifact-reading or principle-reading Ordnungs, controlling for other factors (economic opportunity, family structure).',
    'High relative retention would support treating youth costs as bounded and accepted; markedly lower retention under consequence-reading districts would suggest this reading''s technology restrictions bear disproportionately on the young relative to siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(youth_exit_as_validation_or_indictment, empirical, 'Whether retention data corroborates or challenges the acceptability of costs borne by ambitious youth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__consequence_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__consequence_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__consequence_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__consequence_reading, theater_ratio, 60, 0.1).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__consequence_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__consequence_reading, base_extractiveness, 30, 0.11).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__consequence_reading, base_extractiveness, 50, 0.13).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__consequence_reading, base_extractiveness, 60, 0.14).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__consequence_reading, suppression_requirement, 10, 0.23).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__consequence_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__consequence_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.26).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__consequence_reading, suppression_requirement, 50, 0.27).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__consequence_reading, suppression_requirement, 60, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.1).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'Amish/Old Order separation from the world' per the epsilon-invariance principle. The artifact reading evaluates technology by visible resemblance to worldly objects (highest suppression, most rigid); the principle reading evaluates by functional entanglement with outside systems (intermediate); the consequence reading evaluates by measured effect on visiting, mutual aid, and rootedness (lowest epsilon, most adaptive but most discretion-dependent). Each carries its own epsilon and stakeholder structure; they are linked here because real Old Order communities visibly cluster around one dominant reading per district, and district-to-district variation in technology rulings is best explained by which reading dominates locally, not by degree of general conservatism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
