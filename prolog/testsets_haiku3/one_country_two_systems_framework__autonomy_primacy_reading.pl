% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems: Autonomy-Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   The One Country, Two Systems framework is a contested constitutional
 *   arrangement grounding Hong Kong's political status within the PRC. This
 *   constraint story instantiates the AUTONOMY-PRIMACY READING: Hong Kong
 *   retains substantive authority over civil liberties, judicial
 *   independence, and local governance; mainland intervention in these
 *   domains constitutes treaty violation and international claim. This
 *   reading treats the Sino-British Joint Declaration and Hong Kong Basic Law
 *   as binding constitutional covenants that lock the autonomy boundary and
 *   make violations enforceable through international and domestic courts.
 *   The competing sovereignty-primacy reading denies this: it asserts that
 *   mainland authority cannot be treaty-constrained and that security
 *   imperatives override local autonomy. The balanced-coexistence reading
 *   sits between them, treating the boundary as negotiable. This story models
 *   ONLY the autonomy-primacy frame — its structural implications,
 *   beneficiaries, enforcement mechanisms, and vulnerabilities. The
 *   measurement series track the constraint's drift over ~27 years
 *   post-handover: extraction (mainland intrusion into autonomy domain) rises
 *   gradually from 0.18 to 0.31, theater (performative autonomy maintenance
 *   as actual authority erodes) rises from 0.22 to 0.48, and suppression
 *   (enforcement pressure against autonomy advocates) stabilizes around 0.42
 *   after gradual escalation. The reading is CLAIMED as tangled_rope because
 *   it performs genuine coordination (reconciling sovereignty and autonomy)
 *   while simultaneously extracting authority from local beneficiaries
 *   (mainland determines security interpretation, appoints key officials,
 *   controls extradition).
 *
 * KEY AGENTS:
 *   - Hong Kong residents (civil liberties holders): powerless/constrained — their rights are the protected category but they have no veto on interpretation
 *   - Hong Kong judiciary: organized/constrained — holds the authority to enforce autonomy but operates under escalating pressure from mainland and pro-mainland coalition
 *   - Democratic reform advocates: moderate/constrained — claim the autonomy frame preserves a legitimate reform pathway, but face legal jeopardy and surveillance
 *   - Mainland central authority: institutional/arbitrage — holds sovereignty and enforcement capacity, constrained by treaty only to the extent the autonomy reading is internationally sustained
 *   - International treaty bodies: institutional/analytical — observe and report violations but lack enforcement power over a Security Council permanent member
 *   - Pro-mainland faction: powerful/arbitrage — excluded from this reading's frame; their sovereignty-primacy reading denies the treaty constraints this reading asserts
 *   - International business sector: powerful/mobile — benefits from autonomous legal system and judicial predictability; can arbitrage to other regional hubs if autonomy erodes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.31).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.42).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems: Autonomy-Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '2223ad49-1015-407f-86c2-258348f50e99').
narrative_ontology:cs_kernel_codification('2223ad49-1015-407f-86c2-258348f50e99', fixed_text).
narrative_ontology:cs_authority_grounding('2223ad49-1015-407f-86c2-258348f50e99', extraction).
narrative_ontology:cs_interpretation_layer_present('2223ad49-1015-407f-86c2-258348f50e99').
narrative_ontology:cs_reading_relation('2223ad49-1015-407f-86c2-258348f50e99', one_country_two_systems_framework__one_country_two_systems_sovereignty_primacy, forecloses).
narrative_ontology:cs_reading_relation('2223ad49-1015-407f-86c2-258348f50e99', one_country_two_systems_framework__one_country_two_systems_balanced_coexistence, influences).
narrative_ontology:cs_axiom('2223ad49-1015-407f-86c2-258348f50e99', foundational, treaty_text_binds_sovereign_authority).
narrative_ontology:cs_axiom_status(treaty_text_binds_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('2223ad49-1015-407f-86c2-258348f50e99', treaty_text_binds_sovereign_authority, deontological).
narrative_ontology:cs_axiom('2223ad49-1015-407f-86c2-258348f50e99', foundational, civil_liberties_institutionally_protected).
narrative_ontology:cs_axiom_status(civil_liberties_institutionally_protected, holdable).
narrative_ontology:cs_axiom_grounding('2223ad49-1015-407f-86c2-258348f50e99', civil_liberties_institutionally_protected, deontological).
narrative_ontology:cs_axiom('2223ad49-1015-407f-86c2-258348f50e99', secondary, international_enforcement_capacity_exists).
narrative_ontology:cs_axiom_status(international_enforcement_capacity_exists, holdable).
narrative_ontology:cs_axiom_grounding('2223ad49-1015-407f-86c2-258348f50e99', international_enforcement_capacity_exists, empirically_contingent).
narrative_ontology:cs_reference_frame('2223ad49-1015-407f-86c2-258348f50e99', treaty_locked_autonomy_framework).
narrative_ontology:cs_drift_state('2223ad49-1015-407f-86c2-258348f50e99', contemporary_post_2019_security_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2223ad49-1015-407f-86c2-258348f50e99', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents_civil_liberties).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, independent_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, democratic_reform_advocates).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, mainland_authority_enforcement_capacity).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, centralized_unified_policy_agents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_business_finance).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, international_business_finance).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, treaty_supremacy_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, judicial_review_institutional_autonomy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, international_human_rights_enforceability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold substantive civil rights under the autonomy reading: freedom of press, assembly, speech, religion, protected by courts and treaty. They cannot exit without abandoning family, property, livelihood. They depend on courts and international bodies to defend these rights against mainland reinterpretation. The autonomy reading asserts they are treaty-protected; mainland pressure to weaken rights enforcement creates their primary vulnerability.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents_civil_liberties, beneficiary,
    powerless, biographical, constrained, regional).

% Hold institutional independence to interpret Hong Kong law, apply common-law principles, and review executive action against civil-rights protections. The autonomy reading assigns them authority to declare mainland actions treaty violations. They operate under escalating pressure from mainland (appointment vetting, public criticism, security law redefinition of judicial scope). A judge can resign but institutional replacement usually continues the pressure on the next incumbent.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, independent_judiciary, beneficiary,
    organized, generational, constrained, regional).

% Claim the autonomy framework preserves a live pathway for governance reform (expanded suffrage, accountability, elections). They operate under criminal legal jeopardy for activities (protest, assembly, speech) deemed to challenge national security or sovereignty. Mainland and pro-mainland coalition classify reform advocacy as sedition or separatism. The autonomy reading protects their claim; the sovereignty-primacy reading forecloses it.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, democratic_reform_advocates, beneficiary,
    moderate, generational, constrained, regional).

% Holds ultimate sovereignty and enforcement capacity (security services, PLA garrison, control of entry/exit, extradition authority). The autonomy reading constrains their direct policy-setting and exposes them to treaty-violation allegations. They maintain options to reinterpret the autonomy boundary unilaterally, pass security laws that override Hong Kong jurisdiction, vet judicial appointments, and surveil civil society. Their directionality is high (target); they experience the autonomy reading as extractive-toward-them.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, mainland_central_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% UN human rights bodies, treaty-monitoring committees, and signatory-state governments receive autonomy-violation complaints and can investigate and report. They lack enforcement machinery over a permanent Security Council member. Their role is monitoring and public accountability. The autonomy reading depends on their legitimacy as observers; the sovereignty-primacy reading dismisses them as interference.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Advocates the sovereignty-primacy reading and institutional integration with mainland. They oppose the autonomy framework's constraint on mainland authority and work to reinterpret or override it. They hold significant institutional positions (business, media, government), mainland government backing, and control of appointment and promotional paths. Their exclusion from the autonomy reading does not silence them — they are primary agents pushing the sovereignty-primacy reading.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_mainland_faction, excluded,
    powerful, generational, arbitrage, regional).

% Benefits from Hong Kong's autonomous legal system (common law, independent judiciary, transparent property rights, financial regulation). They pay the cost of uncertainty: threat of autonomy erosion creates instability and incentivizes hedging through alternative hubs (Singapore, Dubai, etc.). They have genuine exit capacity but also genuine benefit from staying. Their continued investment signals confidence in autonomy constraints; their accelerating relocation signals eroding confidence.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_business_finance, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, international_business_finance, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, mainland_central_authority).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The One Country, Two Systems framework solves the coordination problem of reconciling Hong Kong's integration into PRC sovereignty with preservation of the distinctive institutions (common law, free press, civil rights) that developed under colonial administration. Without this framework, mainland would face the choice of imposing unified governance (destroying international business confidence and civil-liberties protections) or separating Hong Kong (contradicting sovereignty claim). The autonomy reading asserts the coordination solution works: both can be satisfied — mainland sovereignty + Hong Kong autonomy — if the boundary is maintained through law and respected through institutional restraint.
% TRANSFER_FUNCTION: Transfers mainland security authority over territorial integrity and foreign relations while ceding policy authority and civil-liberties protection to Hong Kong institutions. The treaty locks this division: mainland commits to not unilaterally overriding civil rights; Hong Kong commits to not using autonomy for separatism or external alliances. The transfer is asymmetric: mainland holds the enforcement capacity to override, but the treaty makes overriding a violation with international consequence. The extracted benefit is mainland's gain from avoiding the legitimacy cost of unified governance; the cost is mainland's constraint on direct authority.
% ABSENT_VOICES: The sovereignty-primacy reading and pro-mainland faction are structural opponents, not absent voices — they are actively present and disputing the autonomy boundary. An absent voice would be citizens trapped in mainland legal system who have no standing to comment on Hong Kong's autonomy (they cannot travel to Hong Kong freely; their views are censored). The framework of autonomy-primacy excludes them not because they lack interest but because mainland authority prevents their participation.
% DISAPPEARANCE_RATIONALE: If the autonomy framework disappeared: Hong Kong's legal system would integrate into mainland structures (security services, policy hierarchy, judicial oversight); civil liberties protections would shift to mainland standards (national security law precedence, party-state alignment); democratic reform advocacy would be classified as sedition; the judiciary would lose independence and become aligned with party authority; international business confidence would collapse and relocation would accelerate; the Basic Law would function as an advisory document, not a binding constraint. The distribution of authority would shift from bifurcated (one country/two systems) to unified (mainland authority supreme). Hong Kong would no longer be a distinctive political entity but an administrative region under unified PRC governance.
% FOUNDING_PROBLEM: How to integrate Hong Kong from British colonial administration into PRC sovereignty while preserving the legal, economic, and social institutions (common law, market economy, civil society) that had developed and that international actors (business, governments) depended on.
% FOUNDING_PROBLEM_CORROBORATION: The autonomy-primacy reading asserts the founding problem remains LIVE and requires the autonomy framework to remain operative to sustain the solution. Hong Kong civil society organizations, international human rights bodies, and independent legal scholars attest that the founding problem (institutional preservation under sovereignty) remains unsolved if autonomy is eroding. Mainland authority and pro-mainland coalition assert the founding problem has been SUPERSEDED by security integration imperatives and unified development requirements — that the original autonomy compromise was a transitional device, not a permanent structural arrangement. The contest is UNRESOLVED: the autonomy reading's own institutions (courts, civil society) continue defending the boundary; mainland's institutional allies (security apparatus, pro-mainland legislature members, appointed officials) continue pushing for reinterpretation. No corroborating party outside the contesting groups has resolved which reading is correct.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).
:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 endpoint) because the autonomy reading ASSERTS that mainland cannot legitimately extract from the autonomy domain — extraction is reframed as violation. The measured extraction reflects mainland actions (security law, judicial vetting, appointment control) that the autonomy reading treats as illegitimate intrusion, not legitimate governance. The measurement trajectory shows gradual increase (0.18→0.31) corresponding to documented instances of mainland intervention in judicial independence, press freedom, and assembly rights. Suppression is moderate (0.42) because enforcement of autonomy rights does not require high coercion at the resident level — rights are codified and courts initially enforced them — but maintaining the autonomy boundary against mainland pressure requires escalating counter-enforcement (civil society mobilization, international complaint, judicial pushback). Theater rises sharply (0.22→0.48) because performative commitment to autonomy (annual commemorations, formal statements, legal ceremonies) increases as the actual autonomy domain contracts — this is the classic piton-drift signature. The claim (tangled_rope) reflects the reading's assertion that the arrangement performs genuine coordination (two systems under one sovereignty) while extracting authority asymmetrically (mainland controls the boundaries of that coordination). The metrics are authored independently of the claim: the measured extraction reflects how much mainland actually intrudes into the autonomy domain, not how much it should intrude under the reading's normative rules.
 *
 * PERSPECTIVAL GAP:
 *   The autonomy-reading beneficiaries (Hong Kong judiciary, civil-liberties residents) experience the constraint as protective: courts can enforce rights, press can publish, assembly can organize. Their directionality is low (beneficiary end), and they perceive low extraction FROM them. But they are exposed to mainland reinterpretation (the sovereignty-primacy reading) and to deterioration of their effective autonomy. The mainland authority seat experiences the constraint as highly extractive-toward-it: it cannot unilaterally override Hong Kong courts, cannot suppress civil liberties without international claim, cannot appoint judges freely. Its directionality is high (target end), and it perceives the constraint as limiting its sovereign authority. The international observer seats see the arrangement as contested: it works if both parties maintain the autonomy boundary; it collapses if one party unilaterally reinterprets it. The business sector experiences it as fragile but valuable: autonomy benefits them enormously (legal predictability, rule of law) but ongoing threat erodes the benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: Hong Kong residents, judiciary, and democratic advocates benefit from the autonomy framework — they get protected rights, institutional independence, and a live reform pathway. Their exit options are constrained (residents cannot easily leave; judges are appointed; advocates face legal jeopardy), so they are locked into the constraint and experience it as protective. Mainland authority is structurally positioned as the constraint's target: the autonomy reading LIMITS its authority to intervene in local domains, forcing it to work through local institutions and negotiate with independent courts. It has high exit capacity (could reinterpret the framework unilaterally, could use security apparatus to override) but the autonomy reading makes that exit a treaty violation and international incident. The international business sector sits near symmetric: they benefit from autonomy (court enforcement, property rights, transparency) but also pay the cost of uncertainty (threat of reinterpretation creates instability and exit risk). Directionality overrides are NOT necessary; the derived directionality from beneficiary/victim + exit options captures the structure accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading asserts that the founding problem (integrating Hong Kong into PRC while preserving local institutions) remains LIVE and requires the autonomy framework to remain operative. The measurement trajectory (theater rising, suppression rising, extraction rising) is consistent with the founding problem being PARTIALLY SOLVED but UNDER THREAT: the autonomy institutions (courts, press, civil society) persist and function, but mainland pressure to reinterpret the boundary is escalating. The constraint is tangled_rope (not snare) because genuine coordination value exists (two systems under one sovereignty is harder than either unitary governance or full separation) AND asymmetric extraction is present (mainland holds veto power over institutional interpretation). The mandatrophy test is whether the autonomy reading's own commitments are internally consistent: does it assert that courts should enforce rights while also acknowledging that courts operate under mainland veto? The answer is yes — the reading holds both: courts SHOULD enforce autonomy, and the reading's structural claim is that they CAN (within limits) because the treaty binds mainland's compliance. This creates a tangled structure: genuine coordination value (two systems require negotiated boundary) + extraction (mainland can override boundary through security law, appointment control, interpretation) + enforcement (courts push back, international complaints, civil society mobilization). The constraint is not a snare because the autonomy reading does not claim all alternatives are suppressed — it asserts they are available and contestable through courts and international forums.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforceability_versus_sovereignty_supremacy,
    'Can a written treaty (Sino-British Joint Declaration, Basic Law) legally constrain mainland authority''s exercise of sovereignty over Hong Kong, or does sovereignty permit unilateral reinterpretation regardless of text?',
    'International Court of Justice advisory opinion on treaty binding force; PRC ratification and compliance with UN human rights treaty obligations; empirical observation of mainland response to international complaints. The test is whether mainland faces costs (diplomatic, reputational, legal) for overriding the treaty boundary.',
    'If treaty binding: the autonomy reading''s legal framework holds and violations are international claims. If sovereignty supreme: mainland can reinterpret at will and the autonomy reading becomes a hostage to mainland''s tolerance. Classification shifts from tangled_rope (treaty-constrained extraction) to snare (extraction with suppressed alternatives) as treaty enforceability erodes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_enforceability_versus_sovereignty_supremacy, conceptual, 'Whether written constitutional covenant can bind sovereign authority over time.').

omega_variable(
    autonomy_boundary_institutional_stability,
    'Do Hong Kong courts retain practical capacity to enforce autonomy boundaries against mainland pressure, or has institutional capacity been eroded below the threshold where legal review is effective?',
    'Empirical observation of judicial decisions on autonomy claims (assembly, press, national security law application); rate of reversal/override of Hong Kong court decisions by mainland apparatus; resource/appointment control over judiciary and pressure on judges. The test is whether courts can issue a binding decision that mainland does not subsequently override.',
    'If courts retain capacity: the tangled-rope classification holds because extraction requires going through local institutions. If capacity erodes: the constraint shifts toward snare because alternatives (appeal through courts) become merely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_boundary_institutional_stability, empirical, 'Whether Hong Kong judiciary retains institutional independence to enforce treaty boundaries.').

omega_variable(
    democratic_reform_pathway_liveness,
    'Is the claim that the autonomy framework preserves a live pathway for democratic governance reform structurally accurate, or has the reform pathway been foreclosed by mainland veto over constitutional amendment and leadership selection?',
    'Observation of mainland response to democratic reform proposals, analysis of appointment procedures and veto points for leadership, empirical evidence of which reform pathways remain contestable vs. categorically forbidden.',
    'If the pathway remains live: the autonomy reading''s beneficiary set includes democratic reformers, and the constraint preserves their organizing space. If the pathway is foreclosed: the reading''s claim that autonomy includes governance self-determination is hollow, and the constraint becomes pure extraction of authority (mainland constrains local choices without consent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_reform_pathway_liveness, empirical, 'Whether democratic reform remains a live autonomy claim or has been unilaterally foreclosed.').

omega_variable(
    sovereignty_primacy_reading_foreclosure,
    'Does the autonomy-primacy reading''s core assertion (treaty limits mainland authority) logically foreclose the sovereignty-primacy reading (mainland cannot be treaty-limited), or do these readings represent incompatible but live interpretative traditions within the constitutional framework?',
    'Jurisprudential analysis: can both readings be held by parties working within the same constitutional tradition, or does one definitively rule the other out? This is a question about the constitutional structure, not about what is desirable.',
    'If foreclosure: the two readings cannot coexist in the same legal system and one must be rejected. If coexistence: the readings represent competing legitimate interpretations and the ongoing contest is constitutional politics, not a settled legal matter. This affects whether the constraint is modeling a constitutional error (autonomy-primacy is wrong) or a constitutional contest (both readings are institutionally live).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_primacy_reading_foreclosure, conceptual, 'Whether autonomy-primacy and sovereignty-primacy readings logically foreclose each other or coexist.').

omega_variable(
    suppression_internalization_dynamics,
    'Is the measured suppression (0.42 endpoint) structural (external barriers to autonomy advocacy: legal risk, surveillance, arrest) or internalized (citizens and judges self-censor and accommodate mainland pressure without external enforcement), and in what proportion?',
    'Post-suppression observation: do advocates continue organizing if legal barriers are removed? Does self-censorship persist? Do judges continue issuing autonomy-protecting decisions if mainland pressure is lifted? Surveys of institutional actors'' beliefs about freedom of action vs. actual constraints. The test is whether suppression persists after the structural mechanism is removed.',
    'If structural: suppression is a policy choice by mainland apparatus and can be modulated by enforcement changes. If internalized: suppression has fused into institutional culture and identity, and removing structural barriers will not restore autonomy function. If mixed: institutional recovery requires both removing structural barriers AND restoring confidence in autonomy framework''s durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_dynamics, empirical, 'Whether suppression of autonomy advocacy is structural coercion or internalized institutional accommodation.').

omega_variable(
    international_enforcement_capacity,
    'Do international human rights bodies and UN treaty monitoring mechanisms have practical enforcement capacity to sanction mainland violation of the autonomy boundary, or does their role remain purely reputational/diplomatic?',
    'Observation of international response to autonomy violations: treaty complaints, investigations, reports, sanctions proposals. PRC response to those reports and sanctions. Economic/diplomatic costs incurred or avoided. The test is whether international enforcement carries teeth or remains advisory.',
    'If enforcement capacity exists: violations are costly and mainland faces incentives for compliance. If enforcement is advisory: mainland can violate without cost and the autonomy reading becomes a call for international reform rather than a description of binding constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_capacity, empirical, 'Whether international treaty enforcement mechanisms carry practical sanctions power or remain advisory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(one__tr_t0, observed).
narrative_ontology:measurement(one__tr_t3, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(one__tr_t3, observed).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement_basis(one__tr_t6, observed).
narrative_ontology:measurement(one__tr_t9, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement_basis(one__tr_t9, observed).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(one__tr_t12, observed).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(one__tr_t15, observed).
narrative_ontology:measurement(one__tr_t18, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 18, 0.47).
narrative_ontology:measurement_basis(one__tr_t18, observed).
narrative_ontology:measurement(one__tr_t21, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 21, 0.48).
narrative_ontology:measurement_basis(one__tr_t21, observed).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 27, 0.48).
narrative_ontology:measurement_basis(one__tr_t27, observed).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(one__be_t0, observed).
narrative_ontology:measurement(one__be_t3, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 3, 0.21).
narrative_ontology:measurement_basis(one__be_t3, observed).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement_basis(one__be_t6, observed).
narrative_ontology:measurement(one__be_t9, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 9, 0.27).
narrative_ontology:measurement_basis(one__be_t9, observed).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 12, 0.29).
narrative_ontology:measurement_basis(one__be_t12, observed).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement_basis(one__be_t15, observed).
narrative_ontology:measurement(one__be_t18, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 18, 0.31).
narrative_ontology:measurement_basis(one__be_t18, observed).
narrative_ontology:measurement(one__be_t21, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 21, 0.31).
narrative_ontology:measurement_basis(one__be_t21, observed).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 27, 0.31).
narrative_ontology:measurement_basis(one__be_t27, observed).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(one__su_t0, observed).
narrative_ontology:measurement(one__su_t3, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 3, 0.31).
narrative_ontology:measurement_basis(one__su_t3, observed).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(one__su_t6, observed).
narrative_ontology:measurement(one__su_t9, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 9, 0.38).
narrative_ontology:measurement_basis(one__su_t9, observed).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(one__su_t12, observed).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(one__su_t15, observed).
narrative_ontology:measurement(one__su_t18, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement_basis(one__su_t18, observed).
narrative_ontology:measurement(one__su_t21, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 21, 0.42).
narrative_ontology:measurement_basis(one__su_t21, observed).
narrative_ontology:measurement(one__su_t27, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 27, 0.42).
narrative_ontology:measurement_basis(one__su_t27, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__autonomy_primacy_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_sovereignty_primacy).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_balanced_coexistence).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_national_security_law_framework).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judicial_independence_boundary).

% DUAL FORMULATION NOTE:
% This is one reading of the One Country, Two Systems kernel. The sibling readings (sovereignty-primacy, balanced-coexistence) instantiate different structural constraints from the same text. Each reading has different beneficiaries, directionalities, and classifications. The kernel contest is not resolved by choosing one reading; all three remain live positions held by different institutional actors (Hong Kong civil society holds autonomy-primacy; mainland authority holds sovereignty-primacy; pragmatists hold balanced-coexistence). The family structure enables the analysis to track how the same constitutional commitment generates different constraint classifications from different seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__autonomy_primacy_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
