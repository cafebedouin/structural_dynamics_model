% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Indigenous Treaty Primacy Over Secession Legitimacy
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story captures the treaty_primacy_reading of the
 *   secession_legitimacy_boundary kernel. The reading asserts that Indigenous
 *   treaty rights — which predate Confederation and are constitutionally
 *   protected under Section 35 — structurally constrain any legitimate
 *   secession process. Neither the federal Parliament nor a provincial
 *   legislature can unilaterally alter treaty relationships or extinguish
 *   treaty rights; any secession claim that proceeds without the free, prior,
 *   and informed consent of affected treaty holders is illegitimate. The
 *   constraint coordinates Indigenous-federal alignment against provincial
 *   secessionism while extracting from separatist movements and provincial
 *   governments that would otherwise claim unilateral exit authority. The
 *   claimed type (tangled_rope) reflects genuine coordination (treaty rights
 *   as constitutional floor) combined with asymmetric extraction (provincial
 *   sovereignty claims blocked, separatist projects constrained).
 *
 * KEY AGENTS:
 *   - indigenous_treaty_holders: Primary beneficiaries and rights-holders (organized/identity_locked) — hold constitutional treaty rights that gate secession legitimacy
 *   - federal_government: Secondary beneficiary and agenda_setter (institutional/arbitrage) — treaty enforcement preserves federal territorial integrity against provincial exit
 *   - provincial_separatist_movements: Primary victims/payers (organized/constrained) — blocked from unilateral secession by treaty consent requirement
 *   - provincial_governments: Secondary victims (institutional/constrained) — territorial sovereignty claims constrained by treaty obligations they did not create
 *   - supreme_court_canada: Observer/agenda_setter (institutional/analytical) — adjudicates treaty scope and secession legality (Reference re Secession 1998, Tsilhqot'in 2014)
 *   - international_law_community: Observer (analytical/analytical) — UNDRIP, FPIC standards inform but do not determine domestic enforceability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.78).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.62).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Indigenous Treaty Primacy Over Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, 'f69d3e76-a261-465f-9cf9-f055405fb4e4').
narrative_ontology:cs_kernel_codification('f69d3e76-a261-465f-9cf9-f055405fb4e4', formalized).
narrative_ontology:cs_authority_grounding('f69d3e76-a261-465f-9cf9-f055405fb4e4', lineage).
narrative_ontology:cs_interpretation_layer_present('f69d3e76-a261-465f-9cf9-f055405fb4e4').
narrative_ontology:cs_reading_relation('f69d3e76-a261-465f-9cf9-f055405fb4e4', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('f69d3e76-a261-465f-9cf9-f055405fb4e4', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('f69d3e76-a261-465f-9cf9-f055405fb4e4', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('f69d3e76-a261-465f-9cf9-f055405fb4e4', foundational, treaty_rights_predate_constitutional_order).
narrative_ontology:cs_axiom_status(treaty_rights_predate_constitutional_order, holdable).
narrative_ontology:cs_axiom_grounding('f69d3e76-a261-465f-9cf9-f055405fb4e4', treaty_rights_predate_constitutional_order, conventional).
narrative_ontology:cs_axiom('f69d3e76-a261-465f-9cf9-f055405fb4e4', foundational, indigenous_consent_required_for_territorial_reorganization).
narrative_ontology:cs_axiom_status(indigenous_consent_required_for_territorial_reorganization, holdable).
narrative_ontology:cs_axiom_grounding('f69d3e76-a261-465f-9cf9-f055405fb4e4', indigenous_consent_required_for_territorial_reorganization, deontological).
narrative_ontology:cs_axiom('f69d3e76-a261-465f-9cf9-f055405fb4e4', secondary, crown_fiduciary_duty_survives_constitutional_amendment).
narrative_ontology:cs_axiom_status(crown_fiduciary_duty_survives_constitutional_amendment, holdable).
narrative_ontology:cs_axiom_grounding('f69d3e76-a261-465f-9cf9-f055405fb4e4', crown_fiduciary_duty_survives_constitutional_amendment, conventional).
narrative_ontology:cs_reference_frame('f69d3e76-a261-465f-9cf9-f055405fb4e4', section_35_constitutional_recognition_1982).
narrative_ontology:cs_drift_state('f69d3e76-a261-465f-9cf9-f055405fb4e4', post_tsilhqotin_undrip_implementation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f69d3e76-a261-465f-9cf9-f055405fb4e4', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_separatist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, treaty_primacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, crown_indigenous_fiduciary_duty).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, section_35_constitutional_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold constitutionally protected treaty rights (Section 35) that predate Confederation. Their consent is structurally required for any legitimate secession affecting treaty territories. They cannot exit the treaty relationship — it constitutes their nationhood and land relationship. The constraint subsidizes their veto power through federal enforcement. Gains: constitutional recognition, consultation rights, resource revenue sharing. Costs: ongoing litigation to enforce rights, Crown's fiduciary duty often unfulfilled.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, beneficiary,
    organized, generational, identity_locked, national).

% Administers treaty obligations and enforces Section 35. Treaty primacy preserves federal territorial integrity against provincial secession — the federal state cannot be dismembered without Indigenous consent. Collects political capital from upholding constitutional order. But also bears enforcement costs (litigation, negotiation, implementation) and faces capture risk: may prefer to minimize treaty obligations while using them instrumentally against secession. Exit: could amend Constitution (extremely difficult) or negotiate treaty modifications (requires Indigenous consent).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, beneficiary).

% Seek unilateral provincial secession (e.g., Quebec sovereignty movement). The treaty primacy constraint blocks their core strategy: Indigenous nations in claimed territory (Cree, Innu, Inuit in Quebec) hold veto via treaty rights and self-determination. Their project is structurally constrained — they cannot achieve legitimate secession without negotiating consent they have historically resisted. Exit from constraint: abandon unilateralism for negotiated consent (identity-threatening for core separatist identity) or challenge treaty validity (legally foreclosed by Section 35 jurisprudence).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_separatist_movements, payer,
    organized, biographical, constrained, regional).

% Claim provincial sovereignty over territory and resources. Treaty primacy constrains their authority: duty to consult, accommodation requirements, revenue sharing, and now secession veto. They did not create treaties (federal Crown did) but bear implementation costs. Their territorial integrity claims are structurally limited by superior treaty rights. Exit: intergovernmental negotiation, litigation (usually losing), or political resistance (e.g., Alberta Sovereignty Act) — all constrained by Supreme Court jurisprudence.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, payer,
    institutional, biographical, constrained, regional).

% Adjudicates the treaty-secession intersection. Reference re Secession (1998) established secession requires constitutional negotiation; Tsilhqot'in (2014) confirmed Aboriginal title and consent requirements. The Court's rulings structurally enforce the constraint but it does not set the agenda — it interprets the kernel. Gains: institutional legitimacy as constitutional arbiter. Costs: political backlash from all sides, legitimacy challenges when rulings constrain popular sovereignty claims.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, supreme_court_canada, observer,
    institutional, generational, analytical, national).

% Provides normative framework (UNDRIP Article 3, 4, 19, 32; FPIC standards) that informs but does not determine domestic enforceability. Canada's UNDRIP implementation (2021) creates interpretive pressure but no direct enforcement. Observes whether domestic constraint meets international standards. No direct stake in Canadian federalism outcome.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_law_community, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional floor for Indigenous-Crown relationships that cannot be unilaterally overridden by federal or provincial secessionist projects. Solves the coordination problem of how pre-existing Indigenous sovereignty interfaces with Canadian federalism: treaty rights operate as a structural constraint that all parties must navigate, preventing a race to the bottom where provinces or federal government extinguish rights for territorial control.
% TRANSFER_FUNCTION: Moves veto authority over secession legitimacy from provincial/federal unilateral claim to Indigenous treaty holders. Transfers political authority: provinces lose unilateral exit option; federal government gains Indigenous alignment against secession but assumes enforcement burden; Indigenous nations gain constitutional veto over territorial reorganization affecting their rights.
% ABSENT_VOICES: Indigenous nations without modern treaties or settled claims (e.g., many BC nations, unceded territories) — their consent requirements are less legally crystallized but politically potent. Urban Indigenous populations off-reserve — excluded from territory-based treaty frameworks. Future generations of all parties — bound by present constitutional settlements without voice. International Indigenous peoples — Canadian precedent influences global standards but they have no seat.
% DISAPPEARANCE_RATIONALE: If treaty primacy over secession vanished overnight, Quebec or other provinces could pursue unilateral secession without Indigenous consent. Indigenous territories would be incorporated into new states without negotiation. Federal territorial integrity would lose its strongest constitutional barrier against dismemberment. The entire architecture of Section 35 jurisprudence (consultation, accommodation, consent, title) would lose its structural anchor. The Canadian federation would reorganize around provincial sovereignty unconstrained by pre-existing Indigenous rights.
% FOUNDING_PROBLEM: Confederation (1867) created a federal union that excluded Indigenous nations and ignored pre-existing treaty relationships. The Constitution Act 1982 (Section 35) was built to constitutionally recognize and affirm existing Aboriginal and treaty rights — solving the founding exclusion by making treaty rights a structural constraint on all Crown sovereignty claims, including secession.
% FOUNDING_PROBLEM_CORROBORATION: Royal Commission on Aboriginal Peoples (1996) — independent federal commission — documented ongoing exclusion and unfulfilled treaties. Truth and Reconciliation Commission (2015) — independent, survivor-led — confirmed founding problem persists in child welfare, justice, resource rights. UNDRIP implementation (2021) — international standard adopted domestically — affirms the problem is live. Federal and provincial governments contest this, citing Section 35 jurisprudence as resolution. The mismatch (status=contested, verdict=world_rearranges) flags unresolved mandatrophy: the arrangement persists and reshapes the world while its founding problem remains disputed.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint blocks a major territorial claim (provincial secession) and redistributes veto power to Indigenous nations — a substantial transfer of political authority. Suppression (0.62) is significant: the constraint requires active judicial enforcement and federal legislative backing to maintain treaty primacy against provincial challenges. Theater ratio (0.18) is low: the coordination function (treaty rights as constitutional constraint) is genuinely operative, not performative. Accessibility collapse (0.45) is moderate: alternatives exist (negotiated secession with consent, constitutional amendment) but are structurally narrowed by the consent requirement. Resistance (0.71) is high: provincial governments and separatist movements actively contest treaty primacy through litigation, political mobilization, and narrative framing. The measurement series shows rising extractiveness and suppression from 1982 (Constitution Act patriation, Section 35) to 2020, reflecting jurisprudential strengthening of treaty rights (Sparrow 1990, Delgamuukw 1997, Haida 2004, Tsilhqot'in 2014) and the 1995 Quebec referendum near-miss that sharpened the secession-treaty intersection.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (separatists, provinces) experience the constraint as extraction from their sovereign claims — a snare-like structure from their view. The beneficiary seats (Indigenous nations, federal) experience it as coordination — a rope-like structure protecting constitutional order. The engine computes this divergence from the declared beneficiary/victim structure and exit options. The gap is not a measurement error; it is the structural reality of a tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty holders are structural beneficiaries (d ≈ 0.15): the constraint subsidizes their veto power, subsidized by federal enforcement. Federal government is a beneficiary (d ≈ 0.25): territorial integrity preserved against provincial exit, though it bears enforcement costs — partial capture risk documented in omega. Provincial separatist movements are full targets (d ≈ 0.85): their core project is blocked, exit from the constraint is identity_locked (secessionism defined by unilateral action). Provincial governments are constrained targets (d ≈ 0.65): their sovereignty claims are limited but they retain substantial authority within treaty boundaries. Supreme Court is analytical (d ≈ 0.5): symmetric costs/benefits of adjudication.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1867-1982): Confederation excluded Indigenous nations; treaty rights were ignored or violated. The 1982 constitutional recognition was built to solve that exclusion. The problem is CONTESTED: Indigenous nations argue the founding problem persists (unfulfilled treaties, unextinguished rights); federal and provincial governments argue the constitutional framework solves it. Corroboration: Royal Commission on Aboriginal Peoples (1996), Truth and Reconciliation Commission (2015), UNDRIP (2016) — all outside the beneficiary set — attest the problem remains live. The constraint has not resolved its mandatrophy; it remains a live coordination-extraction hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is this constraint one reading of a contested kernel (secession_legitimacy_boundary) rather than an independent claim?',
    'The four declared readings (constitutional_impossibility, grievance_threshold, popular_sovereignty, treaty_primacy) each instantiate different constraints with different beneficiary/victim structures. This reading''s distinct victim set (indigenous_treaty_holders if secession proceeds without consent) and beneficiary structure (treaty_holders + federal_government) confirm kernel membership.',
    'If this is a kernel reading, its classification must be generated independently per Rule 1 — ε is reading-indexed over the standing arrangement (unilateral secession attempts), not averaged across readings. Sibling readings are other constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Commitment to kernel structure with sibling readings').

omega_variable(
    treaty_vs_constitutional_supremacy,
    'Does treaty primacy structurally foreclose constitutional_impossibility_reading within a single framework, or do they coexist as competing legitimate positions?',
    'Analyze whether a single legal framework could simultaneously hold: (a) secession requires constitutional amendment, and (b) no amendment valid without Indigenous consent. If both can be true (amendment process exists but is constrained by treaty consent), they coexist. If treaty primacy makes constitutional amendment itself illegitimate for secession, it forecloses.',
    'forecloses reading_relation means the two cannot be held together by any single actor; coexists_with means different actors hold them simultaneously without logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_vs_constitutional_supremacy, conceptual, 'Structural relationship between treaty_primacy and constitutional_impossibility readings').

omega_variable(
    extraction_distribution_uncertainty,
    'Is the constraint''s extraction primarily borne by separatist movements (blocked from unilateral action) or by provincial governments (constrained in territorial claims)?',
    'Track which actors bear enforcement costs and opportunity costs when secession attempts are constrained by treaty consent requirements. Separatist movements face blocked mobilization; provinces face restricted territorial sovereignty claims.',
    'Affects victim set precision and directionality derivation for provincial vs. separatist actors. Different victims imply different χ distributions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_distribution_uncertainty, empirical, 'Whether victims are primarily separatist movements or provincial governments').

omega_variable(
    federal_beneficiary_ambiguity,
    'Is the federal government a genuine beneficiary (treaty enforcement protects federal territorial integrity) or a captured enforcer (administers treaty obligations it would prefer to minimize)?',
    'Compare federal litigation positions: does the Crown actively assert treaty primacy against provinces, or does it resist treaty enforcement while using it instrumentally against secession?',
    'If federal is captured, its beneficiary status is partial; directionality override may be needed. If genuine beneficiary, federal and Indigenous interests align structurally on this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_beneficiary_ambiguity, empirical, 'Federal government''s true structural position relative to treaty enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 1982, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_treaty_primacy_tr_t1982, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1982, 0.08).
narrative_ontology:measurement(secession_treaty_primacy_tr_t1990, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(secession_treaty_primacy_tr_t1995, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2000, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2010, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2020, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2020, 0.18).

% Extraction over time
narrative_ontology:measurement(secession_treaty_primacy_be_t1982, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1982, 0.45).
narrative_ontology:measurement(secession_treaty_primacy_be_t1990, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(secession_treaty_primacy_be_t1995, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(secession_treaty_primacy_be_t2000, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(secession_treaty_primacy_be_t2010, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(secession_treaty_primacy_be_t2020, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(secession_treaty_primacy_su_t1982, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1982, 0.35).
narrative_ontology:measurement(secession_treaty_primacy_su_t1990, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(secession_treaty_primacy_su_t1995, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1995, 0.51).
narrative_ontology:measurement(secession_treaty_primacy_su_t2000, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(secession_treaty_primacy_su_t2010, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(secession_treaty_primacy_su_t2020, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2020, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__treaty_primacy_reading, 0.1).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_resource_revenue_sharing).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, federal_provincial_jurisdiction_disputes).

% DUAL FORMULATION NOTE:
% Constraint family: secession_legitimacy_boundary kernel with four readings. This reading (treaty_primacy) adds Indigenous veto to the secession legitimacy question, structurally distinct from the other three which operate within federal-provincial constitutional framework. The ε values differ: constitutional_impossibility has lower extraction (procedural constraint), popular_sovereignty has near-zero extraction (self-legitimating), grievance_threshold has variable extraction (threshold-dependent). This reading's ε=0.78 reflects the substantial transfer of veto authority to Indigenous nations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__treaty_primacy_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
