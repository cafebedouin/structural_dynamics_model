% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi Rangatiratanga Reading â MÄori Retained Full Authority
 *   domain: constitutional/indigenous_rights/post_colonial
 *
 * SUMMARY:
 *   The MÄori text of the Treaty of Waitangi, Article II, retained tino
 *   rangatiratanga (full authority) for MÄori over their lands, resources,
 *   and taonga, while Article I granted the Crown only kÄwanatanga
 *   (governorship) over British settlers. This reading contests the
 *   English-text sovereignty reading and the moderate partnership reading by
 *   insisting on a strict authority allocation rather than ambiguity or
 *   shared sovereignty. The constraint story models this reading as a
 *   constitutional arrangement that coordinates settler-MÄori coexistence
 *   but has historically operated through massive asymmetric extraction from
 *   MÄori (via Crown violation) and now extracts from settler populations
 *   (via redress and authority-sharing). Claimed as tangled_rope because the
 *   same textual structure both coordinates and reallocates, depending on
 *   which party controls interpretation.
 *
 * KEY AGENTS:
 *   - MÄori collectives (iwi/hapÅ«): Primary beneficiaries (authority retained) and payers (enforcement costs, historical losses) â organised power, identity-locked exit.
 *   - Crown institutions (NZ government): Agenda-setter administering the Treaty relationship and secondary beneficiary (legitimacy) â institutional power, arbitrage exit.
 *   - Settler populations: Primary payers (redress costs, sovereignty-space) â powerful democratic majority, mobile exit.
 *   - Waitangi Tribunal: Analytical observer investigating breaches â institutional, non-enforcing.
 *   - Treaty historians: Analytical observers providing linguistic evidence â analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.62).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi Rangatiratanga Reading â MÄori Retained Full Authority").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional/indigenous_rights/post_colonial").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '8bce392b-cd4b-4e18-b56d-a9dba89da99c').
narrative_ontology:cs_kernel_codification('8bce392b-cd4b-4e18-b56d-a9dba89da99c', fixed_text).
narrative_ontology:cs_authority_grounding('8bce392b-cd4b-4e18-b56d-a9dba89da99c', lineage).
narrative_ontology:cs_interpretation_layer_present('8bce392b-cd4b-4e18-b56d-a9dba89da99c').
narrative_ontology:cs_reading_relation('8bce392b-cd4b-4e18-b56d-a9dba89da99c', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('8bce392b-cd4b-4e18-b56d-a9dba89da99c', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('8bce392b-cd4b-4e18-b56d-a9dba89da99c', foundational, inherent_maori_authority_retained).
narrative_ontology:cs_axiom_status(inherent_maori_authority_retained, holdable).
narrative_ontology:cs_axiom_grounding('8bce392b-cd4b-4e18-b56d-a9dba89da99c', inherent_maori_authority_retained, deontological).
narrative_ontology:cs_axiom('8bce392b-cd4b-4e18-b56d-a9dba89da99c', foundational, crown_jurisdiction_limited_to_kawanatanga).
narrative_ontology:cs_axiom_status(crown_jurisdiction_limited_to_kawanatanga, holdable).
narrative_ontology:cs_axiom_grounding('8bce392b-cd4b-4e18-b56d-a9dba89da99c', crown_jurisdiction_limited_to_kawanatanga, conventional).
narrative_ontology:cs_reference_frame('8bce392b-cd4b-4e18-b56d-a9dba89da99c', maori_authority_intact_1840).
narrative_ontology:cs_drift_state('8bce392b-cd4b-4e18-b56d-a9dba89da99c', contemporary_co_governance_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8bce392b-cd4b-4e18-b56d-a9dba89da99c', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_institutions).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain customary authority over lands, waters, and taonga as guaranteed by the MÄori text of Article II. Must actively assert this authority through Waitangi Tribunal claims, settlement negotiations, and political mobilisation against Crown institutions that assert overarching sovereignty. Bear generational costs of land loss, cultural suppression, and legal expenses resulting from Crown violation of the Treaty, while receiving partial redress and co-governance recognition.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives, payer).

% Administer the Treaty relationship through legislation, the Waitangi Tribunal, and settlement negotiations. Retain de facto sovereignty over most governmental domains while deriving political legitimacy from Treaty partnership rhetoric. Set the rules for how rangatiratanga is recognised, its statutory limits, and the pace of authority transfer. Could theoretically abandon or fully implement the reading through constitutional reform, but faces severe political costs either way.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_institutions, beneficiary).

% Bear diffuse costs of Treaty settlements and co-governance arrangements through taxation, reduced certainty in resource consents, and shared decision-making over traditionally Crown-managed assets. Experience the constraint as either legitimate redress for historical breach or as an illegitimate transfer of democratic sovereignty, depending on political orientation. Can exit partially through emigration or electoral opposition.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_populations, payer,
    powerful, biographical, mobile, national).

% Investigates Crown breaches of the Treaty and recommends remedies. Serves as the primary institutional venue where the rangatiratanga reading is systematically developed and applied to historical claims. Lacks direct enforcement power; recommendations are often ignored or diluted by the Crown.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Provide linguistic and historical evidence about the 1840 meanings of tino rangatiratanga and kÄwanatanga. Their research strengthens the textual basis of the rangatiratanga reading but does not determine political or judicial outcomes.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, treaty_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of how British settlers and MÄori could occupy the same territory without continuous warfare, by allocating distinct spheres of authority â MÄori retain full customary authority over their own lands, resources, and taonga, while the Crown receives limited governorship over British subjects.
% TRANSFER_FUNCTION: Authority, jurisdiction, and resources. MÄori collect recognition of retained authority, partial return of confiscated lands, and co-governance seats; the Crown and settler populations transfer sovereignty-space, tax revenue, and resource access to MÄori collectives through settlements and statutory power-sharing.
% ABSENT_VOICES: Advocates of the English-text sovereignty reading â who hold that MÄori ceded full sovereignty in Article I â are structurally excluded from this reading's framework; their position is treated as a mistranslation rather than a live interpretive option. Radical MÄori independence advocates who reject any Crown presence are also marginalised, as this reading accepts limited Crown governorship over settlers.
% DISAPPEARANCE_RATIONALE: If the rangatiratanga reading disappeared as a constitutional constraint, MÄori authority would collapse into Crown sovereignty, the Treaty settlement framework would lose its grounding, co-governance arrangements would revert to unilateral Crown administration, and the New Zealand constitution would reorganise around Westminster parliamentary supremacy.
% FOUNDING_PROBLEM: How to integrate a large-scale British settler population into Aotearoa without annihilating MÄori political authority or triggering continuous warfare, while the Crown sought imperial expansion and MÄori sought to protect their autonomy and resources.
% FOUNDING_PROBLEM_CORROBORATION: MÄori historians and the Waitangi Tribunal corroborate the coexistence framing from outside the Crown beneficiary seat. Crown historians and parliamentary supremacists attest the problem was establishing orderly British sovereignty. The Tribunal's reports provide institutional corroboration from a seat that does not benefit from Crown extraction.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) is moderate-high because implementing the rangatiratanga reading requires substantial authority and resource transfer from Crown and settler populations to MÄori. Suppression (0.78) is high because the Crown spent most of New Zealand history actively suppressing this reading through warfare, legislation, and judicial interpretation. Theater_ratio (0.65) is moderate-high because contemporary partnership and principles-of-the-Treaty rhetoric often obscures the Crown's retention of ultimate sovereignty. Accessibility_collapse (0.88) is very high because the MÄori text's linguistic distinction between tino rangatiratanga and kÄwanatanga is stark once understood. Resistance (0.85) is very high because Crown and settler institutions have consistently resisted full implementation. The measurement series runs on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The MÄori beneficiary seat experiences this constraint as a protected constitutional authority that has been violated; the Crown agenda-setter seat experiences it as a limitation on democratic sovereignty that must be managed; the settler payer seat experiences it as a cost or as redress. The engine computes these divergences from structural data â the Crown's high power and arbitrage exit lower its effective extraction, while MÄori identity-locked exit amplifies theirs.
 *
 * DIRECTIONALITY LOGIC:
 *   MÄori collectives are declared beneficiaries (authority retained) and victims (historical enforcement costs and ongoing resistance burden), producing a complex directionality that the engine resolves per-seat. Crown institutions are agenda-setter and beneficiary (legitimacy, governorship) with arbitrage exit, placing them near the beneficiary end. Settler populations are payers with mobile exit, placing them mid-target. The structural asymmetry is that MÄori cannot exit the Treaty relationship because it is constitutive of their political identity, while settlers can emigrate or vote to alter the constitutional order.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the rangatiratanga reading as pure extraction (snare) by preserving its genuine coordination function â it solved a real collective-action problem (settler-MÄori coexistence) and both parties received something. It prevents mislabeling it as pure coordination (rope) by acknowledging the massive asymmetric costs MÄori bore through Crown violation of the same structure. The classification captures that the same Treaty text both coordinated and became the vehicle for extraction, depending on which party controlled its interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tino_rangatiratanga_inherent_or_constructed,
    'Is tino rangatiratanga an inherent authority that exists independently of the Treaty (natural law-like), or is it a constructed constitutional claim dependent on the Treaty''s existence?',
    'Comparative analysis of pre-1840 MÄori political authority structures and post-Treaty constitutional claims; examination of whether the authority would persist if the Treaty were repudiated.',
    'If inherent, the constraint approaches mountain status for MÄori and the Treaty is merely declaratory; if constructed, it remains a human agreement subject to renegotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tino_rangatiratanga_inherent_or_constructed, conceptual, 'Whether MÄori authority is inherent or Treaty-dependent').

omega_variable(
    crown_intent_1840,
    'Did British signatories in 1840 understand the MÄori text to limit them to kÄwanatanga, or did they intend full sovereignty regardless of the MÄori text?',
    'Historical archival discovery of British Colonial Office instructions and signatory correspondence; linguistic analysis of contemporary English translations.',
    'If the Crown knew the limitation, the rangatiratanga reading is a straightforward textual interpretation; if not, the reading becomes a more radical reconstructive claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_intent_1840, empirical, 'British signatory intent regarding MÄori text limits').

omega_variable(
    suppression_structural_or_internalized,
    'Is the suppression of the rangatiratanga reading primarily structural (Crown legal and military power) or internalized (generations of MÄori acceptance of Crown sovereignty through colonisation)?',
    'Post-revocation or post-settlement suppression trajectory: if MÄori assertion of rangatiratanga persists or intensifies after structural barriers are removed, suppression was partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression even after formal barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_rangatiratanga_tr_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(waitangi_rangatiratanga_tr_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(waitangi_rangatiratanga_tr_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(waitangi_rangatiratanga_tr_t100, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 100, 0.55).
narrative_ontology:measurement(waitangi_rangatiratanga_tr_t140, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 140, 0.6).
narrative_ontology:measurement(waitangi_rangatiratanga_tr_t184, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 184, 0.65).

% Extraction over time
narrative_ontology:measurement(waitangi_rangatiratanga_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(waitangi_rangatiratanga_be_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(waitangi_rangatiratanga_be_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(waitangi_rangatiratanga_be_t100, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement(waitangi_rangatiratanga_be_t140, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 140, 0.4).
narrative_ontology:measurement(waitangi_rangatiratanga_be_t184, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 184, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_rangatiratanga_su_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(waitangi_rangatiratanga_su_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(waitangi_rangatiratanga_su_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 60, 0.92).
narrative_ontology:measurement(waitangi_rangatiratanga_su_t100, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 100, 0.85).
narrative_ontology:measurement(waitangi_rangatiratanga_su_t140, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 140, 0.7).
narrative_ontology:measurement(waitangi_rangatiratanga_su_t184, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 184, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Treaty of Waitangi sovereignty allocation kernel. It is decomposed from the colloquial label 'Treaty of Waitangi' because the English and MÄori texts, and different interpretive traditions, instantiate structurally distinct constraints with different epsilon values, beneficiary structures, and authority allocations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
