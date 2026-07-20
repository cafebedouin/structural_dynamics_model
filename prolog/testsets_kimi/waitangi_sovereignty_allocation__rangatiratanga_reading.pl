% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Waitangi Treaty Rangatiratanga Reading â MÄori Retained Full Authority
 *   domain: constitutional/political/indigenous_rights
 *
 * SUMMARY:
 *   This constraint instantiates the rangatiratanga reading of the Waitangi
 *   Treaty kernel: the MÄori text of Article II retained tino rangatiratanga
 *   (full authority) over lands, resources, and taonga, while Article I
 *   granted the Crown only kÄwanatanga (governorship) over settlers.
 *   Historically, the Crown has operated under the competing Crown
 *   sovereignty reading, suppressing this interpretation through
 *   parliamentary supremacy, military force, and legal doctrine. The
 *   constraint story captures the structural asymmetry between the treaty's
 *   MÄori text and the Crown's actual exercise of sovereignty over MÄori
 *   populations and territories.
 *
 * KEY AGENTS:
 *   - crown_government: Agenda setter â administers NZ state under Westminster sovereignty, controls Treaty settlement processes and legal enforcement
 *   - maori_iwi_hapu: Primary payer â bear costs of Crown non-recognition of retained authority, locked into lengthy claims processes and ongoing statutory override
 *   - settler_society: Beneficiary â benefits from Crown governance, nationwide property systems, and resource access predicated on exclusive Crown sovereignty
 *   - waitangi_tribunal: Observer â interprets the MÄori text and has supported rangatiratanga readings, but lacks binding authority over the Crown
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.68).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.72).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Waitangi Treaty Rangatiratanga Reading â MÄori Retained Full Authority").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional/political/indigenous_rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a').
narrative_ontology:cs_kernel_codification('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', fixed_text).
narrative_ontology:cs_authority_grounding('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', lineage).
narrative_ontology:cs_interpretation_layer_present('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a').
narrative_ontology:cs_reading_relation('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', foundational, tino_rangatiratanga_retained_not_conferred).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained_not_conferred, holdable).
narrative_ontology:cs_axiom_grounding('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', tino_rangatiratanga_retained_not_conferred, conventional).
narrative_ontology:cs_axiom('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', foundational, crown_jurisdiction_strictly_limited_to_kawanatanga).
narrative_ontology:cs_axiom_status(crown_jurisdiction_strictly_limited_to_kawanatanga, holdable).
narrative_ontology:cs_axiom_grounding('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', crown_jurisdiction_strictly_limited_to_kawanatanga, conventional).
narrative_ontology:cs_reference_frame('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', rangatiratanga_as_constitutional_base).
narrative_ontology:cs_drift_state('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', contemporary_parliamentary_supremacy, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5c092d43-4b35-4e1a-bccb-fb9fd99b6b9a', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_society).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the New Zealand state under Westminster parliamentary supremacy and asserts sovereignty over all territory including MÄori lands. Conducts Treaty settlement processes and consultation while maintaining that full sovereignty was ceded in 1840. Controls the legal and military machinery that determines when MÄori authority is recognized or overridden.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government, agenda_setter,
    institutional, generational, constrained, national).

% Assert customary authority over traditional territories, waterways, and taonga based on the MÄori text of the Treaty. Must engage in lengthy and costly Waitangi Tribunal claims, court litigation, and political negotiation to have authority partially recognized. Bear the ongoing costs of Crown statutes, resource consents, and police jurisdiction exercised over their lands despite the treaty's promise of retained authority.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, beneficiary).

% Benefit from Crown-guaranteed property titles, nationwide infrastructure, and public services delivered under a single sovereignty. Their economic security and access to resources depend on the Crown maintaining exclusive jurisdiction over all New Zealand territory, including areas where MÄori customary authority is contested.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_society, beneficiary,
    organized, biographical, mobile, national).

% Investigates Treaty claims and has issued findings supporting the rangatiratanga reading of the MÄori text, concluding that MÄori did not cede sovereignty. Its recommendations are not binding on the Crown. Operates within the Crown's constitutional and budgetary framework while interpreting the Treaty against Crown actions.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates settler and indigenous populations in Aotearoa New Zealand by allocating distinct spheres of authority: MÄori retain tino rangatiratanga over lands, resources, and taonga while the Crown exercises kÄwanatanga over settler populations and affairs.
% TRANSFER_FUNCTION: Moves jurisdictional authority and resource control from MÄori to Crown institutions in practice, while the MÄori text promises retention. Transfers legitimacy and stability to the Crown by supplying a founding narrative of orderly, consensual colonization.
% ABSENT_VOICES: MÄori factions asserting exclusive jurisdiction without Crown partnership are marginalized in parliamentary and judicial discourse; settlers who would support full MÄori sovereignty are politically dispersed; the British Crown as original treaty signatory is no longer present in the constitutional conversation.
% DISAPPEARANCE_RATIONALE: If this constitutional allocation vanished overnight, Crown legitimacy would collapse, existing property titles and resource consents would face radical legal uncertainty, and the New Zealand state would require fundamental reconstitution. MÄori governance structures would expand to fill jurisdictional vacuums in their traditional territories.
% FOUNDING_PROBLEM: In 1840, British settlers required orderly governance and regulated land acquisition in New Zealand; MÄori leaders required protection from unregulated settler intrusion, lawlessness, and retention of customary authority over their lands and people.
% FOUNDING_PROBLEM_CORROBORATION: The Waitangi Tribunal and independent historians attest that unregulated settlement was a genuine problem. Crown institutions assert the problem was solved by Crown sovereignty. MÄori attest the problem was meant to be solved by dual authority, not by MÄori subordination. No neutral party outside the constitutional dispute corroborates one version exclusively.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is high because Crown institutions continue to extract land, resource, and jurisdictional control from MÄori despite the treaty's promise of retained authority. Suppression (0.72) reflects active Crown maintenance of parliamentary supremacy and judicial doctrines that prevent full rangatiratanga from being operationalized. Theater ratio (0.48) captures the performative dimension of modern Treaty consultation and settlement processes, which simulate recognition while Crown decision-making power remains intact. Accessibility collapse (0.82) is very high because Crown sovereignty has become the nearly unquestioned default within NZ legal and political institutions; the rangatiratanga reading is structurally marginalized. Resistance (0.68) is substantial and ongoing, manifested in protests, occupations, independent MÄori governance initiatives, and litigation. The measurement series tracks the hardening of Crown extraction and suppression through the nineteenth and twentieth centuries, with modest partial reversal since the 1990s.
 *
 * PERSPECTIVAL GAP:
 *   The Crown government seat experiences this constraint as a necessary and legitimate sovereignty arrangement that coordinates settler society and maintains order. The MÄori iwi/hapÅ« seat experiences the same structure as a broken promise that actively extracts their authority and resources. The engine will compute divergent per-seat classifications from this structural data: the Crown seat may compute toward coordination, while the MÄori seat computes toward extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown government and settler society are structural beneficiaries of the Crown sovereignty arrangement that suppresses the rangatiratanga reading; they receive low directionality values, damping their effective extraction. MÄori iwi and hapÅ« are the structural targets: they bear the costs of Crown jurisdiction and resource extraction, hold identity-locked exit options (their political identity is fused to the land and authority the constraint removes), and receive high directionality, amplifying effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and asymmetric extraction for Tangled Rope classification. There is a genuine coordination problem â how do settler and indigenous populations share a territory without continuous violent conflict? The Treaty as a whole, and even this reading, addresses that problem. However, the Crown's insistence on sole sovereignty creates massive asymmetric extraction from MÄori. A pure Snare classification would miss the real coordination function the Treaty was built to solve; a pure Rope classification would miss the ongoing extraction. Tangled Rope captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kawanatanga_sovereignty_semantics,
    'Does the MÄori text''s use of ''kÄwanatanga'' in Article I represent a deliberate cession of full sovereignty, a limited delegation of governorship over settlers, or something else entirely?',
    'Historical-linguistic analysis of 1840 MÄori political vocabulary, including contemporary usage of ''kÄwanatanga'' and ''mana'' in MÄori-language documents and speeches.',
    'If kÄwanatanga encompassed full sovereignty in 1840 MÄori usage, the rangatiratanga reading collapses toward the Crown sovereignty reading. If it denoted limited governorship, Crown sovereignty is structurally incompatible with this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_sovereignty_semantics, empirical, 'The semantic scope of kÄwanatanga at the time of signing').

omega_variable(
    tino_rangatiratanga_scope_boundary,
    'Does tino rangatiratanga cover all MÄori social, political, and legal affairs, or only the lands, estates, and taonga explicitly listed in Article II?',
    'Analysis of pre-1840 MÄori customary authority scope and post-contact political structures.',
    'A broad scope forecloses Crown jurisdiction over most MÄori affairs; a narrow scope permits Crown authority in areas like criminal law and foreign policy, shifting the constraint toward a partnership or coexistence model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tino_rangatiratanga_scope_boundary, conceptual, 'The territorial and functional scope of retained authority').

omega_variable(
    crown_sovereignty_foreclosure_status,
    'Does the rangatiratanga reading logically foreclose the Crown sovereignty reading within any single constitutional framework, or can both be held as layered or segmented authorities?',
    'Formal analysis of whether ''full authority retained'' and ''complete sovereignty ceded'' are logically contradictory, and whether any extant legal theory reconciles them.',
    'If foreclosed, the kernel is zero-sum and the readings are mutually exclusive; if reconcilable, the kernel permits syncretic authority and the readings may influence rather than annihilate each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_sovereignty_foreclosure_status, conceptual, 'Logical relationship between rangatiratanga and Crown sovereignty readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(wait_tr_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(wait_tr_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(wait_tr_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 90, 0.5).
narrative_ontology:measurement(wait_tr_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 120, 0.45).
narrative_ontology:measurement(wait_tr_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 150, 0.52).
narrative_ontology:measurement(wait_tr_t180, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 180, 0.48).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(wait_be_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(wait_be_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 60, 0.8).
narrative_ontology:measurement(wait_be_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 90, 0.85).
narrative_ontology:measurement(wait_be_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 120, 0.82).
narrative_ontology:measurement(wait_be_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 150, 0.72).
narrative_ontology:measurement(wait_be_t180, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 180, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(wait_su_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(wait_su_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(wait_su_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 90, 0.85).
narrative_ontology:measurement(wait_su_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 120, 0.75).
narrative_ontology:measurement(wait_su_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 150, 0.65).
narrative_ontology:measurement(wait_su_t180, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 180, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, partnership_reading).

% DUAL FORMULATION NOTE:
% The Waitangi Treaty kernel decomposes into three structurally distinct readings. This constraint instantiates the rangatiratanga reading, which assigns full retained authority to MÄori and limited governorship to the Crown. It is linked to the Crown sovereignty reading (full cession) and the partnership reading (ongoing shared governance) as sibling constraints within the same kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
