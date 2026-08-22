% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Boundary: Lived Experience and Community Validation as Primary Knowledge Authority
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   The constraint under authorship is the experiential-pluralism boundary
 *   rule: the operative standard by which a community or institution treats
 *   lived experience, ratified through communal validation, as sufficient
 *   ground for legitimate knowledge, with methodological standards admitted
 *   as one tool among many rather than as a required gate. It operates in
 *   patient communities, environmental-justice groups, participatory research
 *   programs, and funded co-production panels. The rule lowers entry barriers
 *   (any member may bring testimony), distributes validation across a
 *   convened community rather than a credentialing profession, and redefines
 *   expertise as context-specific standing rather than methodological
 *   certification. This file instantiates one reading of the
 *   legitimate_knowledge_boundary kernel; the kernel decomposition and
 *   sibling readings are recorded in commentary.kernel_context and the linked
 *   family files. Claim and metrics are authored independently: the reading
 *   presents itself as democratizing coordination, and the metrics below
 *   register both that coordination and the costs the same machinery imposes.
 *
 * KEY AGENTS:
 *   - lived_experience_experts: Primary beneficiary (organized/identity_locked) — holds recognized standing fused with community membership
 *   - community_validation_bodies: Agenda setter (organized/constrained) — administers ratification and recognition
 *   - internal_experience_dissenters: Primary target (powerless/identity_locked) — testimony discounted by the same process that empowers others
 *   - credentialed_researchers: Secondary target (institutional/mobile) — standing demoted, parallel-system exit damps the cost
 *   - grassroots_advocacy_movements: Secondary beneficiary (organized/constrained) — claims ride on the validation process
 *   - participatory_practitioners: Beneficiary-facilitator (moderate/mobile) — designs and runs the machinery, collects fees
 *   - downstream_knowledge_users: Dual-positioned consumer (moderate/constrained) — gains local relevance, bears error risk
 *   - formal_error_correction_bodies: Excluded seat (institutional/arbitrage) — synthesis function sidelined from the conversation
 *   - epistemologists_sts_scholars: Analytical observer — sees the full boundary structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.5).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.43).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.43).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Boundary: Lived Experience and Community Validation as Primary Knowledge Authority").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '25b5c6ce-024c-4c2d-b2c2-c94d811c5b97').
narrative_ontology:cs_kernel_codification('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', distributed).
narrative_ontology:cs_authority_grounding('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', practice).
narrative_ontology:cs_interpretation_layer_present('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97').
narrative_ontology:cs_reading_relation('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', legitimate_knowledge_boundary__credentialed_expertise_reading, forecloses).
narrative_ontology:cs_reading_relation('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', legitimate_knowledge_boundary__hybrid_coproduction_reading, forecloses).
narrative_ontology:cs_axiom('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', foundational, experiential_testimony_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(experiential_testimony_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', experiential_testimony_sufficient_for_legitimacy, deontological).
narrative_ontology:cs_axiom('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', foundational, community_validation_primary_arbiter).
narrative_ontology:cs_axiom_status(community_validation_primary_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', community_validation_primary_arbiter, conventional).
narrative_ontology:cs_reference_frame('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', experiential_primacy_framework).
narrative_ontology:cs_drift_state('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', contemporary_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25b5c6ce-024c-4c2d-b2c2-c94d811c5b97', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, lived_experience_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, grassroots_advocacy_movements).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_practitioners).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, internal_experience_dissenters).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, downstream_knowledge_users).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, downstream_knowledge_users).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_epistemology_doctrine).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, testimonial_justice_principle).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People whose firsthand experience of a condition, place, or practice is formally recognized by their community as a basis for knowledge claims. They speak in validation sessions, serve on community panels, and their testimony carries standing without methodological training. Their epistemic standing is inseparable from their membership and their experience-narrative; stepping outside the community would leave the standing behind.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, lived_experience_experts, beneficiary,
    organized, biographical, identity_locked, national).

% Convene the sessions where experiential claims are heard, apply the community's recognition criteria, and decide which accounts become the community's shared knowledge. Maintain rosters of recognized experience-holders, keep minutes, and handle appeals from members whose testimony was not ratified. Their schedules and criteria effectively set what the community will treat as known.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validation_bodies, agenda_setter,
    organized, generational, constrained, regional).

% Members whose own experience diverges from the account the community has ratified — a different symptom pattern, a different reading of the same event, skepticism toward the consensus narrative. When they speak, their testimony is weighed against the validated account and frequently set aside. Some depend on the community for mutual aid and social ties, and long membership has shaped how they frame their own doubts.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, internal_experience_dissenters, payer,
    powerless, biographical, identity_locked, local).

% Academic and clinical investigators whose findings carry standing in journals and universities but not automatically inside communities operating under this rule. To influence community decisions they must bring their work into validation sessions and argue it alongside experiential testimony. They continue publishing and working in parallel systems, so the demotion taxes their local influence rather than their livelihood.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_researchers, payer,
    institutional, biographical, mobile, global).

% Organized groups built around shared experience — illness, housing, policing, environment — that gained the ability to assert knowledge claims on their own authority. Movement credibility now rests on community validation rather than commissioned studies; leadership invests heavily in maintaining the validation process that underwrites that credibility.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, grassroots_advocacy_movements, beneficiary,
    organized, generational, constrained, national).

% Facilitators, trainers, and co-production consultants who design and run validation processes for communities and institutions. Paid for convening, method design, and conflict handling; their livelihood follows the spread of community-run validation, and through their choice of methods they also shape whose voices the process amplifies.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_practitioners, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_practitioners, agenda_setter).

% Service planners, clinicians, officials, and neighbors who act on the knowledge communities ratify. They gain access to accounts of local conditions that formal studies miss, and they absorb the consequences when a ratified account turns out to be wrong, since few of them can independently check experiential claims against methodological ones.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, downstream_knowledge_users, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, downstream_knowledge_users, payer).

% Systematic review and evidence-synthesis organizations whose function is aggregating and stress-testing claims across sources. Their outputs carry no special standing under this rule; they are not seated in validation sessions and can reach community audiences only by submitting to the same experiential-first weighing as anyone else.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, formal_error_correction_bodies, excluded,
    institutional, generational, arbitrage, global).

% Researchers studying how knowledge boundaries are drawn and contested. They observe validation processes, publish analyses of how standing is granted and withheld, and take no side in which claims the communities ratify.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemologists_sts_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validation_bodies).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a community to generate, ratify, and coordinate around actionable knowledge from its own members' experience when methodological expertise is absent, slow, expensive, hostile, or misaligned with local conditions; maintains a shared account of 'what we know' that members can act on collectively.
% TRANSFER_FUNCTION: Moves epistemic standing — the right to have claims treated as knowledge — from credentialed institutions and methodological publication toward recognized experience-holders and ratifying assemblies; moves attention, deference, and consultative fees toward validation processes and their facilitators.
% ABSENT_VOICES: Formal evidence-synthesis bodies are not seated in validation sessions and would object that cross-source stress-testing is being sidelined; internal dissenters are often present in the room but structurally discounted, which is absence in effect; methodological minorities within communities rarely reach the agenda at all.
% DISAPPEARANCE_RATIONALE: Communities currently coordinating around ratified experience would lose their knowledge-ratification machinery overnight: advocacy campaigns would lose the authority base for their claims, funded participatory programs would lose their decision procedure, and members would fall back on either unstructured opinion or deferred deference to outside experts — a rapid reorganization of who is believed, about what, and on whose say-so.
% FOUNDING_PROBLEM: Credentialing gates excluded people with direct experience of the conditions at issue — patients, residents, workers — while institutional knowledge production ignored, delayed, or misrepresented those conditions; affected communities needed knowledge they could generate and trust themselves.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: histories of medicine and public health documenting exclusion and the Tuskegee-era trust collapse; STS scholarship on lay expertise; institutional admissions by scientific bodies themselves (formal apologies, inclusion mandates); and court and regulatory records in indigenous-knowledge and environmental-justice disputes. The founding problem is attested by historians, scholars, and the excluded parties' own contemporaneous records, not only by the movements the rule now benefits.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.50: the rule's center of gravity is coordination — distributed validation solves a real trust-and-access problem — but the same machinery concentrates ratification authority and discounts divergent testimony, so extraction rides on the coordination rather than replacing it. Suppression 0.43: enforcement is social and procedural (session norms, recognition rosters, appeal denial, mutual-aid dependence) rather than hard coercion, but it binds dissenters whose identity and material life sit inside the community. Theater 0.27: most validation activity is functional, with a growing performative share as funders reward visible participation. Accessibility_collapse 0.30: alternatives persist — journals, universities, and hybrid practices continue alongside — so accepting the rule closes few options outright. Resistance 0.52: credentialed professions contest the demotion and internal dissenters contest ratification outcomes, though both face coordination disadvantages. Coordination type is identity_coordination: the rule maintains the community's knower-boundary — who counts as a source of legitimate knowledge — against evolving membership and criteria, via rosters and reputation; the FNL gaming risk (identity framing covering extraction) is monitored by the relocated_gate_extraction omega. The three temporal series share one grid (points 0-30 at steps of 6) and rise together, modeling institutionalization: informal trust networks hardening into formal boards with procedures, rosters, and funder-facing performance. suppression_requirement is tracked because the story's dynamic is enforcement-capacity maturation, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the validation body's seat the rule is a functioning deliberative institution it staffs and defends. From the lived-experience holder's seat it is long-overdue recognition — subsidy, near-zero effective extraction. From the internal dissenter's seat the same session is a gate: their testimony enters and is set aside, with identity-locked exit, which computes as the highest effective extraction in the story. From the credentialed researcher's seat it is a standing demotion that is costly but survivable — mobile exit damps their effective extraction well below the dissenter's despite both bearing costs. The divergence is the finding: one rule, four experienced constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (lived_experience_experts, grassroots_advocacy_movements, participatory_practitioners) derive low directionality — the rule subsidizes their standing and livelihoods. Victim declarations (internal_experience_dissenters, credentialed_researchers) derive high directionality, modulated by exit: dissenters are identity_locked and powerless, sitting near the full-target end; researchers are institutional and mobile, so their effective extraction is damped well below the dissenter's. The validation body derives near-beneficiary directionality as agenda setter, and the receipt surface names it as the seat the machinery's authority accrues to. Downstream users sit near symmetric: local relevance gained, error risk borne. No directionality overrides were needed — exit modulation plus role declarations separate the seats the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credentialing gates excluding those with direct experience, and institutional knowledge that missed or misrepresented lived conditions — remains live, corroborated from outside the benefiting parties (see six_questions). Status live plus verdict world_rearranges produces no zombie flag: the mandate has not outlived its function. The classification still earns its keep by blocking two symmetrical mislabels: calling the rule a rope (pure coordination) would erase the relocated gate that discounts dissenters; calling it a snare would erase the genuine access and trust function that drew communities to it. Tangled_rope holds both. The rising theater and suppression series flag the early mandatrophy risk: if validation formalizes into ritual ratification of pre-formed positions, the coordination core atrophies while the gate remains — the piton signature — and a later revision of this story should expect theater_ratio past 0.5.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the legitimate_knowledge_boundary kernel — the experiential_pluralism_reading. Which boundary rule governs is itself the contest: how would the constraint''s structure change under the sibling readings?',
    'Track which reading a given institution or community actually operationalizes (admission criteria in charters, validation procedures, appeal paths); classify each site separately rather than averaging across sites.',
    'Under the credentialed_expertise_reading the victim and beneficiary sets invert (unvalidated experiential claims lose standing; credentialing bodies collect); under the hybrid_coproduction_reading neither set inverts but both experiential and methodological claims bear integration costs. The disagreement is located in the admission criterion — what suffices for legitimacy — not in the value of knowledge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer-frame contingency: one reading of a contested kernel; sibling readings change the victim/beneficiary structure.').

omega_variable(
    relocated_gate_extraction,
    'Does community validation dissolve the knowledge gate or relocate it into the community, where dominant voices and founding narratives control ratification?',
    'Comparative audit of ratification outcomes: rates at which divergent testimonies are set aside inside validated communities versus rejection rates under credentialing; transcript analysis of who speaks and who is discounted in validation sessions.',
    'If the gate is relocated rather than dissolved, the measured extraction is intrinsic to this reading and the tangled_rope classification stands; if dissolved, extraction is contingent on particular communities and the constraint sits nearer rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relocated_gate_extraction, empirical, 'Whether distributed validation removes the gate or moves it.').

omega_variable(
    error_correction_capacity_cost,
    'What measurable error-correction cost falls on downstream users when methodological standards are demoted to one tool among many?',
    'Outcome audits of decisions made on communally ratified knowledge versus methodologically validated knowledge in matched domains (treatment choices, remediation plans); track reversal rates over time.',
    'A large reversal-rate gap shifts effective extraction onto downstream users and strengthens drift toward snare; a negligible gap supports the coordination-first reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_correction_capacity_cost, empirical, 'Downstream cost of demoted methodological standards.').

omega_variable(
    dissent_suppression_mechanism,
    'Is the suppression borne by internal dissenters structural (ratification procedures, mutual-aid dependence) or internalized (self-censorship learned through repeated discounting)?',
    'Post-exit trajectory: interview former members who left; if self-censorship and deference to the ratified narrative persist after exit, a substantial share is internalized.',
    'If largely internalized, the scalar suppression understates the constraint''s hold — dissenters carry it with them — and identity_locked exit deepens; if structural, procedural reform (appeals, minority seats) would release it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_suppression_mechanism, empirical, 'Structural versus internalized suppression of internal dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkb_experiential_pluralism_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lkb_experiential_pluralism_tr_t6, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(lkb_experiential_pluralism_tr_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(lkb_experiential_pluralism_tr_t18, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(lkb_experiential_pluralism_tr_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(lkb_experiential_pluralism_tr_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 30, 0.27).

% Extraction over time
narrative_ontology:measurement(lkb_experiential_pluralism_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(lkb_experiential_pluralism_be_t6, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement(lkb_experiential_pluralism_be_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(lkb_experiential_pluralism_be_t18, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement(lkb_experiential_pluralism_be_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(lkb_experiential_pluralism_be_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 30, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(lkb_experiential_pluralism_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(lkb_experiential_pluralism_su_t6, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 6, 0.31).
narrative_ontology:measurement(lkb_experiential_pluralism_su_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(lkb_experiential_pluralism_su_t18, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 18, 0.37).
narrative_ontology:measurement(lkb_experiential_pluralism_su_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(lkb_experiential_pluralism_su_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 30, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'whose knowledge counts?' decomposes, per the epsilon-invariance principle, into three structurally distinct boundary rules with different epsilon, beneficiary, and victim sets: the credentialed_expertise_reading (incumbent, upstream — its institutional weight is what the experiential reading formed against), this experiential_pluralism_reading (corrective, downstream), and the hybrid_coproduction_reading (mediating synthesis). Each is authored as its own file with its own stable epsilon; this file authors epsilon only for the experiential-communal rule. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
