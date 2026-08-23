% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av Commemorative Obligation — Mourning-Practice Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   The annual fast of the Ninth of Av obligates community members to a full
 *   day of fasting, abstention, and night-long recitation of elegies for the
 *   destroyed temples and the catastrophes subsequently folded into the day.
 *   This story instantiates ONE reading of the catastrophe_memory_function
 *   kernel: the mourning_practice_reading, which holds that the ritual
 *   preserves mourning-practice and boundary-norms (D1/D4) and NOTHING else —
 *   the ritual IS the boundary-maintenance, with no survival-competence
 *   transmission. Per the epsilon-invariance principle, the sibling readings
 *   (survival_competence_reading, hybrid_transformation_reading) are separate
 *   constraints with their own files, their own epsilon, and their own
 *   stakeholder surfaces; they are linked only through network edges and the
 *   committer omegas. KEY AGENTS (by structural relationship): -
 *   observant_community_members: primary bearers of the obligation
 *   (organized/identity_locked) — pay the annual fasting and grief-labor,
 *   receive the identity good - rabbinic_authorities: agenda-setting
 *   custodians (institutional/identity_locked) — fix the calendar, adjudicate
 *   exemptions, authorize the liturgy, and collect concentrated standing from
 *   custody - occasional_participants: episodic bearers (moderate/mobile) —
 *   pay intermittently, receive intermittently - secular_unaffiliated_jews:
 *   boundary-marked outsiders (organized/mobile) — covered by the liturgy's
 *   collective voice, excluded from the obligation -
 *   reform_movement_institutions: intra-traditional dissenters
 *   (organized/mobile) — hold the standing precedent that the obligation is
 *   revocable - ritual_scholars: analytical observers (analytical/analytical)
 *   — describe the structure without bearing it Claim/metric independence:
 *   the claimed_type (rope) states what this reading believes is structurally
 *   true — genuine identity coordination with modest extraction — while the
 *   metrics state what is descriptively true of the arrangement's operation
 *   (epsilon 0.30, suppression 0.40, concentrated custodial gain). Where the
 *   engine computes divergent per-seat types from that combination, the
 *   divergence is the datum, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - observant_community_members: primary bearers (organized/identity_locked) — pay fasting and grief-labor annually, receive belonging and structured grief; exit means leaving the identity-world, not skipping a day
 *   - rabbinic_authorities: agenda-setting custodians (institutional/identity_locked) — administer calendar, exemptions, and liturgy; collect concentrated legitimacy from custody they cannot relinquish without dissolving their standing
 *   - occasional_participants: episodic bearers (moderate/mobile) — fast some years, skip others; bear episodic cost with weak steady receipt of the identity good
 *   - secular_unaffiliated_jews: excluded boundary-marked outsiders (organized/mobile) — would object that binding commemoration converts shared memory into a membership test
 *   - reform_movement_institutions: excluded dissenters (organized/mobile) — hold the nineteenth-century abolition precedent establishing the obligation's revocability
 *   - ritual_scholars: analytical observers (analytical/analytical) — study liturgical development and memory-function from outside the obligation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.3).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av Commemorative Obligation — Mourning-Practice Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'aba2c209-bbf8-4344-a1b2-900130aae6ed').
narrative_ontology:cs_kernel_codification('aba2c209-bbf8-4344-a1b2-900130aae6ed', formalized).
narrative_ontology:cs_authority_grounding('aba2c209-bbf8-4344-a1b2-900130aae6ed', lineage).
narrative_ontology:cs_interpretation_layer_present('aba2c209-bbf8-4344-a1b2-900130aae6ed').
narrative_ontology:cs_reading_relation('aba2c209-bbf8-4344-a1b2-900130aae6ed', catastrophe_memory_function__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('aba2c209-bbf8-4344-a1b2-900130aae6ed', catastrophe_memory_function__hybrid_transformation_reading, forecloses).
narrative_ontology:cs_axiom('aba2c209-bbf8-4344-a1b2-900130aae6ed', foundational, no_survival_competence_transmission).
narrative_ontology:cs_axiom_status(no_survival_competence_transmission, holdable).
narrative_ontology:cs_axiom_grounding('aba2c209-bbf8-4344-a1b2-900130aae6ed', no_survival_competence_transmission, empirically_contingent).
narrative_ontology:cs_axiom('aba2c209-bbf8-4344-a1b2-900130aae6ed', foundational, memory_obligation_constitutes_community).
narrative_ontology:cs_axiom_status(memory_obligation_constitutes_community, holdable).
narrative_ontology:cs_axiom_grounding('aba2c209-bbf8-4344-a1b2-900130aae6ed', memory_obligation_constitutes_community, deontological).
narrative_ontology:cs_reference_frame('aba2c209-bbf8-4344-a1b2-900130aae6ed', commemorative_boundary_maintenance).
narrative_ontology:cs_drift_state('aba2c209-bbf8-4344-a1b2-900130aae6ed', contemporary_post_sovereignty, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aba2c209-bbf8-4344-a1b2-900130aae6ed', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, observant_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, observant_community_members).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, occasional_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fast for a full day each year, abstain from bathing, anointing, and leather footwear, sit on low stools or the floor, and recite the elegies (kinot) and Lamentations through the night and morning. They bear the annual grief-labor and compliance cost and receive in return belonging, a structured container for inherited loss, and membership in a community whose calendar this day anchors. Leaving the observance is not skipping a day; it is stepping outside the identity-world the day helps constitute, so exit is fused with self-description rather than blocked by an external barrier.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, observant_community_members, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, observant_community_members, beneficiary).

% Fix the calendar date, adjudicate exemptions (illness, nursing mothers, travelers), compile and authorize the elegy collections, and rule on how new catastrophes enter the day's liturgy. Custody of the mourning obligation is the source of their communal standing: they administer what the community experiences as its oldest continuous practice. Relinquishing custody would dissolve the authority that custody confers, so the role and the self are fused.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities, beneficiary).

% Fast or attend the elegies in some years — when a family anniversary, a crisis in Israel, or social proximity makes the day salient — and skip it in others without rupture. They bear the episodic cost of the fast and receive episodic meaning, but they do not hold the identity good steadily enough for the annual obligation to return much to them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, occasional_participants, payer,
    moderate, immediate, mobile, global).

% Stand outside the obligation's binding reach but inside its boundary-drawing: the day's liturgy mourns on behalf of a collective whose losses they share ancestry to, while its administration marks them as not fully of the mourning community. They would object that commemoration run as binding obligation converts shared memory into a membership test, and they have built parallel rites — state memorial ceremonies, secular yahrzeit practice — that carry some of the same memory without the obligation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, secular_unaffiliated_jews, excluded,
    organized, generational, mobile, global).

% Formally abolished the fast in the nineteenth century as incompatible with emancipated citizenship and the claim that the exile had ended; partially restored it in the late twentieth century reframed as historical memory rather than binding obligation. They contest the obligation's bindingness, not the memory itself, and their nineteenth-century ruling remains the standing intra-traditional precedent that the obligation is revocable by communal decision.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, reform_movement_institutions, excluded,
    organized, generational, mobile, continental).

% Study the fast's liturgical development, its absorption of successive catastrophes, and its function in collective-memory formation. They describe the structure from outside the obligation, bear none of its costs, and collect no rents from its operation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes grief across geographically dispersed communities on a fixed calendar date; supplies a shared script (fast, prohibitions, elegies) so inherited loss is processed collectively rather than idiosyncratically; and draws the boundary of the mourning community through common practice — the doing of the ritual together is itself the act that maintains who belongs.
% TRANSFER_FUNCTION: Moves compliance-labor (fasting, night-long attendance, performed grief) from individual members to the collective identity-project; moves interpretive authority and custodial standing upward to the rabbinic layer that administers the day; and moves memory-obligation downward from generation to generation of the observant.
% ABSENT_VOICES: Secular and unaffiliated Jews, whom the liturgy's collective voice implicitly covers while its obligation excludes; Reform institutions, whose abolition ruling contests the obligation's bindingness; women in the many communities and centuries where they were obligated in the fast but barred from leading the elegies; and the mourned dead themselves, whose loss is administered annually without any voice in how it is framed.
% DISAPPEARANCE_RATIONALE: If the obligation vanished overnight, the communal year would lose its anchor of gravity: the grief-infrastructure that processes inherited catastrophe would close, the boundary between observant and secular would need redrawing around other markers, rabbinic custodial standing would lose one of its oldest pillars, and successor rites (state memorial days, secular ceremonies) would absorb part but not all of the function — the specific obligation-structure would not regenerate quickly because it accretes over centuries.
% FOUNDING_PROBLEM: After the destructions of 586 BCE and 70 CE the community lost temple, sovereignty, and territorial center; the arrangement was built to keep that loss present on a fixed annual date so a dispersed and defeated people would retain coherence, distinctiveness, and orientation toward return.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the benefiting parties: Zionist historiography and Israeli state practice attest the sovereignty-restoration reading (the founding problem transformed if not resolved); academic liturgical historians document the folding-in of post-1945 catastrophes as evidence the problem was made live again rather than having died; and secular Jewish civil society attests that the identity-function persists independent of the original theological frame. No party inside the beneficiary set is the sole attester.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.30 at interval end): the obligation costs members a full day of fasting and grief-labor each year, decoupled from any service rendered in return, but the identity good returned is real and the committed core is plausibly net-positive. Suppression (0.40) reflects social and internalized enforcement rather than a dedicated coercive apparatus — congregational pressure, marriage-market and burial consequences at the margins, and self-description fused with observance. Theater is low (0.16) because the performance IS the function under this reading: the staged communal grief is the boundary-maintenance mechanism, so performative and functional activity largely coincide; the small residual is habitual attendance among marginal participants. Accessibility collapse is moderate (0.45): secular memorial rites, therapy, and parallel ceremonies remain available and partially substitutable, so alternatives do not vanish once the constraint is understood. Resistance (0.28) is real but bounded — the Reform abolition, ongoing secular indifference, and the Zionist obsolescence-critique — never rising to systemic challenge because the practice demonstrably delivers the coordination good to its core.
 *   
 *   The suppression_requirement series is authored deliberately: the story tracks an enforcement-capacity DECAY arc, from pre-emancipation kehillah self-governance that could compel observance (0.58 in 1800) through emancipation's dissolution of coercive communal power to the modern regime of voluntary, socially-sanctioned observance (0.40). This is an enforcement-machinery erosion narrative, which is exactly the case the scalar rule reserves for the temporal series. All three tracked metrics run on one shared grid (1800, 1850, 1900, 1945, 1945-era inflection at 1945, 1970, 2000, 2025) so no metric row borrows another's end-state values. Extractiveness dips slightly at 1850 (the Reform challenge forced justification and briefly disciplined custodial rent-seeking) and rises after 1945 as the folding-in of the Holocaust expanded the grief-labor demanded and raised custodial stakes.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the committed member's position the arrangement is a rope they would largely re-choose: identity-locked exit coexists with genuine net benefit, and the obligation reads as the price of a self they endorse. From the occasional participant's position the same structure operates as episodic extraction — cost paid in years of salience, good received too thinly to amortize it. From the rabbinic custodian's position the day is sacred trust and stewardship; the concentrated legitimacy it confers is invisible as gain because it is constitutive of the role. From the excluded seats the day is a boundary-drawing instrument that speaks in their ancestors' losses while marking them outside. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: observant_community_members and rabbinic_authorities are declared beneficiaries, pulling both toward the subsidized end — correct for the members' net position, and nearly correct for the custodians. Two overrides correct places where the derivation would misread the structure. First, institutional -> 0.22: the custodians derive from their beneficiary declaration a d near the pure-beneficiary end, but they are also the seat the extraction demonstrably accrues to (gain_flow names them) — administering the obligation others pay for is the mechanism by which compliance converts into standing, so their d sits above the derived value. Second, moderate -> 0.62: occasional_participants carry no beneficiary or victim declaration, so they would fall to the canonical fallback; the story establishes they bear episodic cost while receiving only episodic benefit, placing them well past symmetric toward the target end. Members' identity_locked exit keeps them nearer the trapped end than their beneficiary declaration alone would suggest, which the structural derivation registers through exit modulation without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (statelessness and centered loss) is contested rather than dead: sovereignty was restored in 1948, which the Zionist critique reads as resolving the mandate, while the folding-in of subsequent catastrophes reads as renewing it. Because founding_problem_status is contested (not dead) alongside disappearance_verdict world_rearranges, the mismatch consumer finds no dead-mandate-plus-rearrangement flag, and the low theater ratio corroborates that persistence is functional rather than inertial — observance broadened after 1945 rather than hollowing. The classification prevents two opposite mislabels: calling this a snare would erase the genuine coordination function that the committed core demonstrably values; calling it a pure rope would hide the custodial capture (concentrated gain at the agenda-setter seat) and the exclusion cost imposed on boundary-marked outsiders. Omegas marginal_participant_net_position and exclusion_cost_attribution hold both errors open for resolution rather than letting the aggregate net-benefit claim settle them by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of the catastrophe_memory_function kernel — the mourning_practice_reading asserting pure D1/D4 (mourning-practice and boundary-norms, no survival-competence transmission). What would the sibling readings change structurally?',
    'Author the sibling stories (catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading) and compare epsilon, beneficiary/victim sets, and computed types across the family.',
    'If the hybrid reading is correct that adaptive content is transmitted, this reading''s exclusivity axiom fails and the coordination function widens beyond boundary-maintenance; if the survival reading is correct, the constraint is better modeled as competence-transmission infrastructure with an entirely different beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: this story is one of three readings of the catastrophe-memory kernel, distinguished by an exclusivity axiom.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.40) structural (congregational pressure, marriage-market and burial consequences, communal sanction) or internalized (members experience non-observance as self-betrayal)?',
    'Post-exit trajectory of leavers: if distress and perceived transgression persist after leaving the observant community, the internalized share is high; if they resolve quickly, the structural share dominates.',
    'If internalized, effective suppression exceeds the structural measure — the constraint travels with the leaver — raising per-seat extraction for identity_locked members and shifting their computed type away from rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of communal observance pressure between external sanction and fused self-description.').

omega_variable(
    marginal_participant_net_position,
    'Are occasional participants net beneficiaries of the identity good or net payers of compliance cost?',
    'Panel data on participation motives and post-participation valuation across affiliation levels, comparing years of salience-driven observance against years of lapse.',
    'If marginals are net payers, effective extraction at the moderate-power seat rises and the computed type shifts toward tangled_rope despite the committed core''s net benefit — the aggregate rope claim would rest on a survivorship selection over who remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_participant_net_position, empirical, 'Net position of low-affiliation participants under the annual obligation.').

omega_variable(
    founding_problem_obsolescence,
    'Did sovereign restoration (1948) resolve the founding problem (statelessness and centered loss), or has the folding-in of subsequent catastrophes kept it live?',
    'Comparative observance and liturgical data before and after 1948, particularly the composition of post-1945 elegy collections and whether added catastrophes function as renewal or as substitution for the original object of mourning.',
    'If resolved, the constraint drifts toward piton — a mandate outliving its function, maintained by inertia and custodial interest; if transformed-and-live, the rope classification holds and the post-1945 growth in observance is functional rather than theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, conceptual, 'Obsolescence status of the memorial mandate under sovereignty restoration.').

omega_variable(
    exclusion_cost_attribution,
    'Does the constraint''s extraction include the cost imposed on excluded Jews — being marked outside the mourning community whose collective voice the liturgy claims to speak in?',
    'A conceptual decision on the accounting boundary: restrict epsilon to obligated participants, or extend it to boundary-marked non-participants whose relationship to the community the ritual redefines annually.',
    'Extending the boundary raises epsilon materially and pushes classification toward tangled_rope; restricting it keeps the rope claim viable but leaves the exclusion cost unaccounted in the constraint''s ledger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_cost_attribution, conceptual, 'Accounting boundary for who counts as extracted-upon by a boundary-maintaining ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1800, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(cata_tr_t1850, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(cata_tr_t1900, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(cata_tr_t1945, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1945, 0.13).
narrative_ontology:measurement(cata_tr_t1970, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(cata_tr_t2025, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 2025, 0.16).

% Extraction over time
narrative_ontology:measurement(cata_be_t1800, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1800, 0.22).
narrative_ontology:measurement(cata_be_t1850, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1850, 0.2).
narrative_ontology:measurement(cata_be_t1900, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1900, 0.23).
narrative_ontology:measurement(cata_be_t1945, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1945, 0.26).
narrative_ontology:measurement(cata_be_t1970, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1970, 0.27).
narrative_ontology:measurement(cata_be_t2000, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 2000, 0.29).
narrative_ontology:measurement(cata_be_t2025, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 2025, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1800, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1800, 0.58).
narrative_ontology:measurement(cata_su_t1850, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1850, 0.52).
narrative_ontology:measurement(cata_su_t1900, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1900, 0.47).
narrative_ontology:measurement(cata_su_t1945, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1945, 0.44).
narrative_ontology:measurement(cata_su_t1970, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(cata_su_t2000, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement(cata_su_t2025, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the natural-language label 'what Tisha B'Av preserves' covers three structurally distinct claims. This file authors the mourning_practice_reading (pure D1/D4, epsilon 0.30, identity-coordination beneficiaries, rope claim). The survival_competence_reading authors the claim that the ritual transmits adaptive capacity (D5) — different beneficiaries (institutions and successors rather than the mourning community), different epsilon, different failure modes. The hybrid_transformation_reading authors the conjunction. Each story gets its own stable epsilon and stakeholder surface; the family is linked through network edges and the committer omegas rather than by averaging readings inside one constraint. Upstream/downstream: this reading is upstream of the hybrid in the sense that the hybrid must defend its D5 component against this reading's exclusivity axiom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, institutional, 0.22).
constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
