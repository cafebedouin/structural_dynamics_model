% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe-Commemoration Ritual Cycle — Hybrid Atrophy Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A community that historically suffered recurrent catastrophic floods and
 *   fires maintains an annual commemorative ritual cycle: processions along
 *   the old escape routes, liturgical recitation of water-signs and granary
 *   rules, offering obligations, feast duties, and a trained officiant
 *   lineage. On the hybrid_atrophy_reading instantiated here, the cycle was
 *   built to transmit survival competence across generations — the procession
 *   taught the escape geography, the liturgy encoded threat indicators and
 *   stockpile discipline, and the obligatory calendar guaranteed each cohort
 *   rehearsed before the next catastrophe. Under modernity (engineered
 *   levees, professional forecasting, state evacuation management) that
 *   operational payload has decayed to vestigial references few participants
 *   can decode, while the obligations themselves persist at full strength;
 *   the rite now chiefly performs mourning and in-group continuity, and its
 *   costs fall on a present generation that inherits the burden without the
 *   adaptive payoff its founders purchased. This story authors THAT reading
 *   only, as a clean epsilon-invariant constraint: epsilon is assessed for
 *   the standing arrangement — the ritual obligation complex as practiced
 *   today — by this reading's own lights, never for the revived-survival or
 *   purely-symbolic arrangements the sibling readings describe. The sibling
 *   readings are separate constraints, linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   ritual_council_of_elders: Agenda-setter (organized/identity_locked) —
 *   administers the cycle, could reform it, collects status and role
 *   continuity, bears stewardship costs - present_generation_participants:
 *   Primary payer with secondary beneficiary position (moderate/constrained)
 *   — bears time, dues, feast and grief obligations; receives belonging -
 *   emigrant_younger_generation: Payer-turned-leaver (moderate/arbitrage) —
 *   bore the burden through upbringing, exited to cities -
 *   secular_emergency_management_agencies: Excluded actor
 *   (institutional/mobile) — runs the functional replacement, holds no seat
 *   in the ritual conversation - ritual_scholars: Analytical observer
 *   (analytical/analytical) — documents the rite from outside its economy
 *
 * KEY AGENTS:
 *   - ritual_council_of_elders: agenda_setter with secondary beneficiary position — organized power, identity_locked exit, generational horizon, regional scope; administers every element of the cycle and could change it
 *   - present_generation_participants: payer with secondary beneficiary position — moderate power, constrained exit, biographical horizon, regional scope; bears the annual obligations and receives belonging
 *   - emigrant_younger_generation: payer — moderate power, arbitrage exit, biographical horizon, national scope; carried the burden through upbringing and left
 *   - secular_emergency_management_agencies: excluded — institutional power, mobile exit, generational horizon, national scope; supplies the functional replacement from outside the ritual economy
 *   - ritual_scholars: observer — analytical power, analytical exit, civilizational horizon, continental scope; documents the rite without standing in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.36).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe-Commemoration Ritual Cycle — Hybrid Atrophy Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '41d4af8a-9827-4677-9a9c-cb4dbf425e0d').
narrative_ontology:cs_kernel_codification('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', implicit).
narrative_ontology:cs_authority_grounding('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', practice).
narrative_ontology:cs_interpretation_layer_present('41d4af8a-9827-4677-9a9c-cb4dbf425e0d').
narrative_ontology:cs_reading_relation('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', foundational, rite_originally_encoded_operational_competence).
narrative_ontology:cs_axiom_status(rite_originally_encoded_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', rite_originally_encoded_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', foundational, operational_transfer_decayed_to_negligible_under_modernity).
narrative_ontology:cs_axiom_status(operational_transfer_decayed_to_negligible_under_modernity, holdable).
narrative_ontology:cs_axiom_grounding('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', operational_transfer_decayed_to_negligible_under_modernity, empirically_contingent).
narrative_ontology:cs_reference_frame('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', operational_survival_transmission_regime).
narrative_ontology:cs_drift_state('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', contemporary_engineered_safety_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('41d4af8a-9827-4677-9a9c-cb4dbf425e0d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_council_of_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_participants).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_participants).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, emigrant_younger_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit and administer the annual commemorative cycle: fixing the calendar, training successors, reciting the liturgy, adjudicating disputes over proper observance. Their standing in the community rests on the offices they hold, and several spend most of their discretionary time on the rite. Changing or retiring any element would require them to declare publicly that what they have devoted their lives to transmitting is no longer what it was; leaving the role would mean relinquishing the identity around which their lives are organized.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_council_of_elders, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_council_of_elders, beneficiary).

% Attend the annual cycle, prepare offerings, host feast obligations, bring children to rehearsals, and contribute upkeep dues. What they receive is belonging, a place in the community's story, and the annual homecoming; what they give is time, money, and the emotional labor of inherited grief. Skipping years carries gossip, family disappointment, and a sense of betraying the dead. Moving away loosens but does not sever the pull, and those who stay nearby find opting out costlier than complying.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_participants, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_participants, beneficiary).

% Grew up inside the obligation — rehearsed processions, memorized litanies, funded feasts from first wages — and later left for cities where the ritual calendar does not follow them. They send remittances home and return for major anniversaries, but the yearly weight lifted the day they settled elsewhere. Their absence reads to the elders as erosion and to themselves as relief.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, emigrant_younger_generation, payer,
    moderate, biographical, arbitrage, national).

% Run the levees, forecasts, warnings, and evacuation drills that handle the floods and fires the rite once trained people for. They have no seat in the ritual assembly and rarely engage with it; when consulted, they note that preparedness budgets crowd out easily and that commemorative spending is not theirs to redirect. Their operations are fully independent of the rite.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, secular_emergency_management_agencies, excluded,
    institutional, generational, mobile, national).

% Ethnographers and historians of religion who document the rite's archive, interview participants across generations, and compare it with neighboring communities' practices. They publish analyses the community rarely reads and hold no standing in its decisions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes the community on an annual remembrance calendar: gathers dispersed members at fixed times, assigns mourning roles, reproduces group boundaries and the shared catastrophe narrative across generations, and provides a recurring assembly that anchors homecoming and mutual-aid expectations.
% TRANSFER_FUNCTION: Moves time, dues, offerings, feast-hosting labor, and attention from present-generation households (and, during upbringing, from eventual emigrants) to the ritual apparatus — site upkeep, ceremonies, feasts, officiant support — and into diffuse symbolic goods: identity continuity, communal standing, and the council's stewardship roles.
% ABSENT_VOICES: Hazard scientists and emergency planners would object that commemorative expenditure crowds out preparedness training and would redirect it; assimilated younger members would object to compulsory inherited grief; lapsed families would object to the social pricing of non-attendance. They are absent because the council sets the ritual agenda, dissent is priced as disrespect to the dead, and objectors' standard move is exit rather than voice — the assembly hears mainly from those who stayed.
% DISAPPEARANCE_RATIONALE: No operational capacity would be lost — on this reading the survival payload is already gone — but the social world would rearrange: the annual homecoming would dissolve, the calendar's anchor point would vanish, family visit patterns and the site economy would reorganize within a few cycles, and the community would need new machinery, or accept fragmentation, for collective mourning and boundary maintenance.
% FOUNDING_PROBLEM: Recurrent catastrophic floods and fires killed community members unpredictably and repeatedly; the rite was built to encode and transmit survival competence — escape geography, threat signs, stockpile and timing discipline — so each cohort inherited tested responses instead of relearning them through mortality.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: pre-modern parish and temple mortality ledgers recording catastrophe deaths that cease after engineered controls arrive; insurance and municipal archives; and the natural experiment of neighboring communities that abandoned the rite without measurable loss of catastrophe survival once professional emergency services covered them. Ethnographic literature documents the liturgy's practical annotations falling out of maintenance. No party inside the ritual economy attests that the founding problem is still live except by restating continuity values.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.48 and declining (series 0.62 to 0.48 across the 120-year interval): the obligations remain real — annual time, dues, feast duties, inherited grief — but their weight falls as enforcement decays and exit becomes ordinary, and this reading prices the arrangement's cost against a payoff that has already lapsed. Suppression (scalar 0.36, series 0.55 to 0.36) is authored as a raw structural property, unscaled by power or scope: communal sanction and family expectation, decaying across the interval as the community pluralizes; the enforcement-decay trajectory is the dynamic this story tracks, which is why a suppression_requirement series is authored at all. Theater_ratio rises 0.34 to 0.67, crossing 0.5 mid-interval: as the operational payload decayed, the share of ritual activity that is performative maintenance rather than functional transmission rose accordingly — the Goodhart signature of a proxy (observance itself) replacing the goal (competence). Accessibility_collapse is low (0.30): secular substitutes exist and work — memorial services, museum curation, school curricula, professional drills — so seeing the rite's condition does not foreclose alternatives. Resistance is moderate-low (0.38): quiet attrition, skipped years, emigration, occasional explicit refusal, rather than organized opposition. The annual liturgical calendar is the practice's rhythm, not metric oscillation, so no cyclical series is authored. Claimed type piton is authored from the structural reading (former function atrophied, persistence by inertia, no capturing seat); the metrics are authored descriptively and independently of that claim. All three series share one seven-point grid so the engine samples aligned rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from one structure. From the council's seat the cycle is a living inheritance they steward at personal cost — low experienced burden, high meaning, and reform unthinkable because it would repudiate their life's work. From the resident participant seat the same obligations arrive as cost with fading explanatory power — payable, but increasingly answered with 'why.' From the emigrant seat, post-exit, the apparatus reads as dead weight they are relieved to have set down. The excluded emergency-planning seat sees misallocated preparedness resources. The engine computes these per-seat classifications from the power, exit, and role data; the divergence between the elder seat's lived continuity-experience and the participant and emigrant seats' experienced burden is the perspectival fact this story encodes, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary and victim declarations map onto real positions. The council collects status and role continuity (declared beneficiary, identity_locked — directionality near but not at the beneficiary end, since the office also costs them stewardship labor). Resident participants are dual-listed: they pay the obligations and receive belonging, so their derived directionality sits mid-to-high, moderated by the identity subsidy and by constrained exit. Emigrants are declared victims whose arbitrage-grade exit places them at the mobile end of exit modulation, damping their effective extraction after departure even though they bore the full weight during membership. The excluded agency and the scholarly observer sit outside the transfer and carry no directional stake in it. No directionality_overrides are authored: the beneficiary/victim declarations plus the exit atoms already yield the intended directionalities, and the dual-positioned participant seat is expressed through its secondary_role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting catastrophe-survival competence to each new cohort in a world of recurrent floods and fires — is dead: engineered hydrology, meteorological forecasting, and state emergency management absorbed the function at far lower mortality cost. The arrangement persists regardless, sustained by inertia, identity, and the council's inability to reform it cheaply. The classification work this story performs is boundary-keeping: the decaying enforcement series and the diffuse gain flow bar a snare reading (no seat captures the extraction; coercion is waning, not ratcheting); the lapsed payoff bars a rope reading (the costs no longer buy the coordination good they were built to buy); the rising theater ratio together with the dead-founding-problem/world-rearranges mismatch marks the zombie signature the piton category exists to name. The council could change the rite — it administers every element — but the fix (publicly admitting the function is gone, restructuring or retiring the cycle) costs more than the council bears, which is the piton cost-asymmetry in one sentence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is authored as the hybrid_atrophy_reading of the catastrophe_memory_preservation kernel — the claim that the rite''s survival-competence function was historically real but has decayed under modernity. What would the sibling readings change structurally?',
    'Read the sibling stories (catastrophe_memory_preservation__survival_competence_reading and catastrophe_memory_preservation__mourning_practice_reading) alongside this one and compare their epsilon values, beneficiary/victim sets, and computed types; the delta across the three files is the kernel''s contest made concrete.',
    'Under the survival_competence_reading the same observances carry low epsilon (a live adaptive function whose costs are coordination costs) and compute nearer rope; under the mourning_practice_reading the atrophy framing drops, resistance falls, and the costs read as chosen expressive expenditure rather than inherited burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one of three readings of the catastrophe-memory kernel.').

omega_variable(
    operational_residue_question,
    'Does any operational threat-recognition transfer survive in the rite — can participants still decode the vestigial content (procession routes as escape geography, liturgical water-sign and granary references), or is the operational layer fully inert?',
    'Knowledge-testing of participants against hazard scenarios; comparison of liturgical route references with viable modern evacuation geography; archival tracing of when the rite''s practical annotations stopped being maintained.',
    'Residual transfer would shift this reading toward the survival sibling and lower effective epsilon; confirmed inertness fixes the referent of the measured cost as ceremonial dead-weight with no adaptive remainder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_residue_question, empirical, 'Whether the operational layer of the rite is fully inert or retains decodable residue.').

omega_variable(
    atrophy_or_transformation,
    'Is the present state terminal atrophy (a former function kept alive by inertia) or a completed transformation into a genuinely new function (mourning and identity coordination that pays its own way)?',
    'Willingness-to-pay and revealed-preference study: if members defend the rite''s cost as worthwhile expressive expenditure independent of any survival framing, the state is transformation; if defense depends on unexamined continuity assumptions and flounders when the historical function is surfaced, the state is atrophy.',
    'A transformation verdict reclassifies toward rope-like identity coordination with modest inherent cost; an atrophy verdict confirms the piton profile and the dead-mandate mismatch flag.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_or_transformation, conceptual, 'Terminal atrophy versus successful functional transformation of the rite.').

omega_variable(
    internalized_obligation_share,
    'How much of the compliance pressure on present-generation participants is structural (communal sanction, family expectation enforceable in daily village life) versus internalized (filial duty and guilt toward the dead that would persist if sanction vanished)?',
    'Post-exit trajectory of emigrants: if observance obligations continue to bind leavers remotely (remittances earmarked for the rite, guilt-driven anniversary returns exceeding stated preference), the internalized share is large.',
    'A dominant internalized share means the scalar suppression understates the arrangement''s grip — enforcement decay would not release participants — and strengthens the atrophy reading over the transformation reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_obligation_share, empirical, 'Structural versus internalized share of compliance pressure on participants.').

omega_variable(
    true_cost_magnitude,
    'What is the full annual cost of observance to a median household — time, dues, feast-hosting obligations, emotional labor of inherited grief — relative to community means?',
    'Household time-use and expenditure surveys comparing participating and lapsed families; valuation of feast obligations and rehearsal hours.',
    'Calibrates epsilon: a small burden would make the moderate epsilon an overestimate (affordable identity overhead); a large burden would mean the declining trend masks a heavy residual levy on households that no longer receive the adaptive good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_magnitude, empirical, 'Magnitude of the residual observance burden relative to household means.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.34).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.58).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.63).
narrative_ontology:measurement(cata_tr_t120, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 120, 0.67).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.51).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.49).
narrative_ontology:measurement(cata_be_t120, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 120, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement(cata_su_t120, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 120, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the catastrophe ritual preserves memory' covers three structurally distinct claims with different epsilon values and different failure modes, authored as three linked stories. The survival_competence_reading is the upstream strong claim (if operational transfer is live, the observances' costs are coordination costs and epsilon is low); the mourning_practice_reading is the downstream symbolic-only claim (no operational assertion, costs read as expressive expenditure); this hybrid_atrophy_reading mediates historically, asserting a real origin and a decayed present, which yields moderate declining epsilon and the piton profile. Each story links to the other two; the upstream claim is typically cited as warrant by the hybrid reading's defenders, which is why the influence edge runs through the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
