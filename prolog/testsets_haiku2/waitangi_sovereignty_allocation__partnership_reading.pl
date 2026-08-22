% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Reading: Crown-Māori Consultation and Protection Obligation
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is a foundational document of New Zealand
 *   governance read differently by Crown and Māori. The partnership reading
 *   interprets the Treaty as establishing an ongoing Crown-Māori relationship
 *   requiring good faith consultation and Crown protection of Māori
 *   interests, despite the text's ambiguity and the Crown's initial assertion
 *   of unilateral sovereignty. This reading was formally articulated through
 *   the Waitangi Tribunal (established 1975) and has become doctrine through
 *   court decisions and settlement practice. Under this reading, the Crown
 *   has a legal and moral obligation to consult Māori before decisions
 *   affecting taonga, lands, and Māori interests, and to consider Māori input
 *   seriously—though consultation is not veto power. The constraint enables
 *   ongoing negotiation of the sovereignty question without resolving it, but
 *   operates asymmetrically: the Crown retains ultimate authority while Māori
 *   gain voice and redress through settlements and consultation rights. The
 *   partnership reading coexists with two other readings (crown sovereignty
 *   and rangatiratanga) that would produce different constraint types and
 *   distribution of authority.
 *
 * KEY AGENTS:
 *   - Crown Executive: Sets consultation protocols, determines scope and weight of consultation, can reframe partnership as advisory
 *   - Māori Communities (organized through iwi): Gain voice through consultation and settlement but bear costs of litigation to enforce rights; constrained exit
 *   - Treaty Settlement Beneficiaries: Receive redress and co-management rights as recognition of partnership principle
 *   - Parliament: Retains legislative supremacy; can override consultation obligations but faces political constraint
 *   - Courts and Waitangi Tribunal: Interpret partnership principle and enforce procedural obligations
 *   - Settler Constituencies: Benefit from Crown authority moderated by consultation; minimal direct cost
 *   - Rangatiratanga Advocates (excluded): Reject partnership reading as Crown-friendly compromise; argue for retained Māori sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.62).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.58).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Reading: Crown-Māori Consultation and Protection Obligation").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'cad5bbf4-0319-4aee-8866-0b5f4e4adc27').
narrative_ontology:cs_kernel_codification('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', fixed_text).
narrative_ontology:cs_authority_grounding('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', lineage).
narrative_ontology:cs_interpretation_layer_present('cad5bbf4-0319-4aee-8866-0b5f4e4adc27').
narrative_ontology:cs_reading_relation('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', foundational, ongoing_crown_maori_partnership_obligation).
narrative_ontology:cs_axiom_status(ongoing_crown_maori_partnership_obligation, holdable).
narrative_ontology:cs_axiom_grounding('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', ongoing_crown_maori_partnership_obligation, conventional).
narrative_ontology:cs_axiom('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', foundational, good_faith_consultation_requirement).
narrative_ontology:cs_axiom_status(good_faith_consultation_requirement, holdable).
narrative_ontology:cs_axiom_grounding('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', good_faith_consultation_requirement, deontological).
narrative_ontology:cs_reference_frame('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', partnership_doctrine_framework).
narrative_ontology:cs_drift_state('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', contemporary_period_2020_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cad5bbf4-0319-4aee-8866-0b5f4e4adc27', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_communities).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_beneficiaries).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, settler_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Crown executive (Minister of Treaty Negotiations, Cabinet, departmental heads) sets consultation protocols, determines which decisions trigger consultation obligations, and decides the weight to give Māori input after consultation occurs. Formally bound by the partnership reading but retains final decision-making authority. Can reframe consultation as advisory while claiming the obligation is satisfied.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_executive, agenda_setter,
    institutional, generational, analytical, national).

% Gain a recognized voice in decisions affecting lands, resources, taonga (sacred things), and self-determination through consultation requirements and principles-doctrine constraints on Crown action. Simultaneously bear the cost of litigation to enforce consultation rights, experience delayed decision-making while consultation occurs, and face the asymmetry that consultation input can be overridden by Crown authority. Their exit options are constrained by territorial, cultural, and institutional dependence on the Crown-administered legal system.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_communities, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, maori_communities, beneficiary).

% Iwi and hapū that have concluded formal Treaty settlements receive redress (land, cash, cultural recognition) and co-management rights over certain resources and decision processes. They benefit from the partnership reading's enforcement mechanisms (Crown duty to consult, to consider Māori interests) but their gains are tied to negotiated settlements, not the reading's core operation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_beneficiaries, beneficiary,
    moderate, biographical, constrained, national).

% Retains formal legislative supremacy; can legislate to override consultation obligations or redefine the partnership principle, though politically constrained by public commitment to Treaty relationship. The partnership reading does not constrain parliamentary sovereignty in law, only in legitimacy and political feasibility.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, parliament, observer).

% Interpret and enforce the partnership reading through judicial review (assessing whether consultation occurred, was genuine, and was considered) and through declaration of principles. Courts cannot override Parliament but can impose procedural remedies and symbolic recognition of breaches. The Waitangi Tribunal supplements court jurisdiction by inquiring into Crown breaches and recommending remedies.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, courts, observer,
    institutional, generational, analytical, national).

% Non-Māori New Zealanders benefit indirectly from Crown authority and policy, and from the constraint's moderation of Māori claims by way of consultation and settlement frameworks rather than systemic recognition of Māori sovereignty. They face no direct cost from the partnership reading's operation (consultation extends timelines and resource costs minimally) and can exit specific policy domains through electoral politics.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, settler_constituencies, beneficiary,
    powerful, biographical, mobile, national).

% Māori individuals and whānau not formally represented through iwi structures or settlement entities are outside the consultation architecture. The partnership reading applies consultation obligations but provides no mechanism for direct individual voice independent of iwi gatekeeping. Their exclusion from formal consultation structures is structural to the arrangement.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, excluded_maori_voices, excluded,
    powerless, biographical, trapped, local).

% Māori and allied advocates who read the Treaty as establishing retained Māori sovereignty (the rangatiratanga reading) rather than Crown-Māori partnership reject the partnership reading as a Crown-friendly compromise. They contest the legitimacy of the constraint itself from outside the partnership framework, seeing consultation as insufficient and the constraint as consolidating Crown authority rather than constraining it.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a structured mechanism (consultation obligation, principles doctrine, settlement framework) for addressing competing claims of authority (Crown sovereignty vs. Māori tino rangatiratanga) without resolving the underlying constitutional question. Enables ongoing negotiation of resource rights, cultural recognition, and Māori participation in governance without requiring Parliament to cede sovereignty or Māori to abandon rangatiratanga claims. Solves the coordination problem of coexistence without constitutional settlement.
% TRANSFER_FUNCTION: Transfers recognition, voice, and redress to Māori communities in exchange for acceptance of Crown authority as binding in law (even if not in principle). Moves settlement resources (land, cash, cultural authority over specific domains) to beneficiaries of negotiated settlements. Moves consultation time and process costs to the Crown and to Māori through litigation and negotiation overhead. Moves legitimacy from pure parliamentary sovereignty to a framework claiming partnership.
% ABSENT_VOICES: Rangatiratanga advocates argue the partnership reading forecloses full Māori sovereignty and constrains the conversation to Crown-acceptable frameworks; they are excluded from the consultation architecture unless their iwi entity has settled. Excluded Māori (non-iwi-affiliated, dissenting iwi members) lack formal voice because consultation routes through iwi structures. These absent voices would argue the constraint legitimates partial measures instead of enabling structural change.
% DISAPPEARANCE_RATIONALE: If the partnership reading and its consultation obligations vanished, Crown policy-making would cease to recognize Māori consultation as a procedural requirement; settlements would revert to legislative discretion; principles doctrine would lose enforceability. Māori would lose institutional channels for voice in governance and resource decisions, though the underlying dispute over sovereignty would persist. The constraint structures how the dispute is managed, not whether it exists.
% FOUNDING_PROBLEM: After 1840 Treaty signature, two parties claimed authority over the same territory under incompatible interpretations: the Crown read the English version as ceding complete sovereignty; Māori read the Te Reo version as retaining tino rangatiratanga. One hundred fifty years of Crown unilateral action, land confiscation, and cultural suppression created a legitimacy crisis. The partnership reading was developed (articulated formally through the 1975 Waitangi Tribunal Act and principles doctrine) to acknowledge the Crown's duty to Māori while preserving Crown authority, providing a framework for addressing historical grievances without resolving the sovereignty question.
% FOUNDING_PROBLEM_CORROBORATION: The Waitangi Tribunal, New Zealand courts (most notably in cases like Ngāi Tahu [2012] and Whanganui River decisions), international human rights bodies (UN Permanent Forum on Indigenous Issues), and independent constitutional scholars attest that the founding problem persists: the Crown and Māori remain in constitutional dispute, and the partnership reading is the operative framework for managing it. The Crown's own Treaty settlement and consultation policies affirm the founding problem's continuing relevance. Rangatiratanga advocates and Māori rights organizations contest whether the partnership reading adequately addresses it.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The partnership reading is classified as TANGLED ROPE because it performs genuine coordination (both parties benefit from structured negotiation avoiding violence) while sustaining extraction (Crown retains ultimate authority despite consulting; Māori pay litigation costs and face consultation that can be overridden). Extractiveness at 0.62 reflects this asymmetry: Māori are consulted but not co-sovereign; settlements provide redress for past wrongs but do not restructure ongoing Crown authority. Suppression at 0.58 reflects moderate coercion: the constraint is enforced through court orders and institutional pressure, though courts cannot override Parliament; rangatiratanga advocates are excluded from the framework altogether. Theater at 0.41 reflects moderate performative maintenance: consultation occurs and is sometimes substantive, but is also used symbolically to legitimize decisions already made. The measurement series shows extractiveness rising from t=0 to t=32, then plateauing: the constraint's asymmetry has stabilized (Crown authority is entrenched while consultation rights are established). Theater rises similarly through t=40 then slightly declines, suggesting increased emphasis on the performative aspect of consultation. Suppression rises modestly, reflecting slight intensification of enforcement (more Tribunal inquiries, more court challenges, more departmental consultation protocols) without systematic change. One shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The partnership reading's core asymmetry is that it claims to establish partnership while preserving Crown sovereignty. The Crown reads this as partnership (shared commitment to consultation and good-faith engagement within a framework of Crown authority). Māori read it as constrained acknowledgment (the Crown admits obligation but retains veto). Rangatiratanga advocates read it as foreclosure (accepting partnership forecloses the claim to retained sovereignty). The partnership reading produces DIFFERENT classifications at different seats: Crown→ROPE (genuine mutual coordination), Māori→TANGLED ROPE or SNARE (extraction dressed as partnership), Rangatiratanga advocates→SNARE (constraint that legitimates Crown authority while foreclosing full Māori sovereignty claims).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim naming requires careful parsing. Māori are BOTH because the constraint genuinely requires the Crown to consider Māori interests (a benefit not present without the partnership reading) AND genuinely preserves Crown's ultimate authority (a cost not present if the rangatiratanga reading were operative). The asymmetry arises because: (1) Māori benefits are conditional (consultation → Crown consideration, but Crown can override); (2) Crown benefits are unconditional (authority is preserved regardless of consultation outcome); (3) exit is asymmetric (Crown can legislate around consultation; Māori must litigate to enforce it). Directionality derivation: Māori have 'organized' power and 'constrained' exit, which sets their d higher (toward target) than a mobile actor in the same beneficiary position would be. Crown has 'institutional' power and 'analytical' exit (can legislate), which sets their d lower (toward beneficiary) despite nominally being agenda-setter, because they can escape constraints through legal redefinition. The engine derives this from the structural atoms without manual tuning.
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership reading does NOT exhibit mandatrophy in the classical sense because the founding problem (the sovereignty dispute) remains live and the constraint addresses it functionally. However, there is a performance-of-legitimacy component: consultation rituals may substitute for substantive power-sharing, and settlement negotiations may be framed as partnership implementation while structurally consolidating Crown authority. Theater ratio at 0.41 captures this partial drifting toward performance. The constraint avoids mandatrophy classification by remaining tied to the founding problem's persistence—courts continue to enforce consultation rights, settlements continue to be negotiated, and the partnership reading continues to constrain Crown behavior even if imperfectly. The monitoring regime (Waitangi Tribunal inquiries, judicial review, political pressure) preserves functional connection to the founding mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partnership_vs_supremacy_ambiguity,
    'Is the partnership reading a genuine joint authority arrangement, or does it merely legitimate Crown supremacy by providing consultation without co-decision?',
    'Systematic analysis of Crown consultation outcomes: what percentage of consultation input materially changes Crown decisions? Does the partnership reading systematically bias toward Crown positions even when Māori input contradicts them? Comparative institutional analysis: do partnership arrangements in other post-colonial constitutions (Canada, Australia) show similar asymmetries?',
    'If consultation is systematically overridden, the constraint reclassifies from TANGLED ROPE (hybrid coordination and extraction) toward SNARE (pure extraction with consultation as legitimation theater). If consultation materially affects outcomes, the TANGLED ROPE classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partnership_vs_supremacy_ambiguity, empirical, 'Whether partnership is substantive joint authority or legitimizing consultation').

omega_variable(
    reading_foreclosure_boundary,
    'Does the partnership reading logically foreclose the rangatiratanga reading, or do they coexist as incompatible claims held by different parties within the same constitutional system?',
    'Logical analysis: can a court or legislator hold both readings simultaneously (applying partnership within the Crown administrative system while acknowledging Māori claims to retained rangatiratanga)? Or do they logically contradict? Empirical test: do Māori advocates hold both readings in tension, or do they identify as partisan for one reading only?',
    'If they coexist: the constraint is a COEXISTS_WITH relation to the rangatiratanga reading. If the partnership reading forecloses rangatiratanga (by affirming Crown authority while dispensing consultation), then FORECLOSES is the relation. This affects how the two constraints interact in the network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether partnership reading logically excludes rangatiratanga or coexists with it').

omega_variable(
    consultation_substantiveness_drift,
    'Has Crown consultation practice systematically drifted toward performative (satisfying the obligation without substantive consideration) rather than deliberative?',
    'Process audit: examine case samples of Crown consultation from early partnership-doctrine era (1975–1995) vs. contemporary period (2015–2025). Measure: proportion of consultations where Crown position changed materially in response to input; time allocated; documentation of consideration; Tribunal findings on consultation adequacy.',
    'Rising theater ratio and evidence of performative drift would suggest the constraint is suffering from Goodhart displacement (the measure of consultation—process completion—becomes the target, replacing the underlying objective of genuine partnership consideration). This would support PITON reclassification if theater approaches or exceeds 0.6.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consultation_substantiveness_drift, empirical, 'Whether consultation practice is becoming increasingly performative over time').

omega_variable(
    institutional_identity_lock,
    'To what extent are Māori iwi leaders identity-locked into the settlement/consultation framework such that exiting to pursue rangatiratanga claims would require dismantling their professional role?',
    'Ethnographic / institutional analysis of iwi leadership structures: what proportion of iwi leadership income and status derives from settlement administration and Crown consultation roles? Post-settlement identity interviews: do iwi leaders report feeling professionally and institutionally bound to the partnership framework? Do exit alternatives (e.g., direct assertion of rangatiratanga without Crown negotiation) feel available or foreclosed?',
    'If identity lock is substantial, Māori directionality is higher (more trapped/identity-locked than ''constrained''), which increases effective extraction. The constraint sustains itself partly through institutional identity fusion, not merely through legal enforcement. Breaking the constraint would require identity-frame dissolution as well as legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock, empirical, 'Degree to which iwi leadership is identity-fused with settlement framework').

omega_variable(
    parliament_override_readiness,
    'How ready is Parliament to legislate away consultation obligations? What political barriers prevent unilateral Crown override of the partnership reading?',
    'Legislative history analysis: instances where Parliament considered overriding consultation requirements and what political consequences followed. Public opinion polling: settler support for Treaty obligations and consultation. Electoral analysis: whether electoral punishment for Treaty rollback is material enough to constrain political parties.',
    'If Parliament faces real electoral/political costs for override, Crown authority is more constrained by the partnership reading than raw legal supremacy suggests. If override is easy, Crown supremacy is the real law and partnership is legitimatory theater. This affects whether Crown directionality should be ~0.25 (constrained beneficiary) or ~0.0 (full beneficiary with consultation as window dressing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliament_override_readiness, empirical, 'Whether political constraints make Crown override of consultation genuinely costly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(wait_tr_t8, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(wait_tr_t16, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(wait_tr_t24, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(wait_tr_t32, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(wait_tr_t40, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(wait_tr_t50, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(wait_be_t8, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(wait_be_t16, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(wait_be_t24, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(wait_be_t32, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(wait_be_t40, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(wait_be_t50, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(wait_su_t8, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(wait_su_t16, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(wait_su_t24, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(wait_su_t32, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(wait_su_t40, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(wait_su_t50, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__partnership_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi is a single stabilized kernel with three structurally distinct readings instantiated as three separate constraints. The partnership_reading (this story) establishes Crown-Māori partnership with consultation obligations, asymmetrically favoring Crown authority while providing Māori voice and redress. The crown_sovereignty_reading reads Article I as ceding complete sovereignty, establishing Westminster supremacy; the rangatiratanga_reading reads Te Reo Article II as retaining Māori tino rangatiratanga. Each reading has its own ε (extractiveness from that reading's standpoint), its own beneficiary/victim structure, and its own classification. They form a constraint family linked via network.affects_constraints. The partnership reading coexists with the other two as live positions held by different constituencies within the same political system; it does not foreclose them logically but does create institutional pressure toward settlement within the partnership framework rather than toward either pure Crown supremacy or restored Māori sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
