% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__commons_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__commons_stewardship_reading, []).

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
 *   constraint_id: ietf_openness_commitment__commons_stewardship_reading
 *   human_readable: IETF Openness Commitment — Commons Stewardship Reading (Open Standards as Public Infrastructure)
 *   domain: technology governance/internet standards/institutional economics
 *
 * SUMMARY:
 *   The IETF's openness commitments — open working-group participation, the
 *   rough-consensus decision procedure, freely published specifications, and
 *   the RFC 2026/8179 licensing framework asking contributors to grant
 *   royalty-free or reasonable-and-nondiscriminatory licenses — constitute a
 *   standing arrangement constraining everyone who touches internet protocol
 *   standardization. This file instantiates ONE reading of that contested
 *   kernel (ietf_openness_commitment): the commons_stewardship_reading, under
 *   which the arrangement is public infrastructure that solves the
 *   fragmentation collective-action problem, constrains large and small
 *   implementers equally toward interoperability, and carries no structural
 *   beneficiary class. Epsilon's referent is the standing arrangement itself,
 *   assessed by this reading's lights: near-floor extraction consisting of
 *   coordination overhead rather than rent. The sibling readings —
 *   capture_substrate_reading (resource advantage translates to encoded
 *   gatekeeping; would declare a resource-rich beneficiary class and author
 *   far higher epsilon) and legitimacy_erosion_reading (the rough-consensus
 *   mechanism itself as contested and capture-vulnerable; would relocate the
 *   constraint to the consensus procedure with elevated suppression) — are
 *   separate constraint files linked via network.affects_constraints. The
 *   three readings disagree about the same artifacts; the disagreement is
 *   located in whether participation asymmetry converts into outcome bias and
 *   whether the procedural safeguards actually bind. KEY AGENTS (by
 *   structural relationship): - iesg_iab_stewardship: Agenda-setter
 *   (institutional/mobile) — administers the process, enforces IPR disclosure
 *   and consensus rules, collects no material rent -
 *   large_vendor_implementers: Cost-bearing beneficiary
 *   (institutional/mobile) — bears disclosure and conformance costs, receives
 *   interoperability access at scale - small_implementers_open_source:
 *   Primary beneficiary (moderate/mobile) — receives specification access and
 *   interoperability without bearing process costs - internet_end_users:
 *   Diffuse beneficiary (powerless/constrained) — inherits the interoperable
 *   network the constraint preserves - exclusive_ipr_holders: Excluded voice
 *   (powerful/arbitrage) — would prefer royalty-bearing standardization;
 *   cooled out by the licensing-commitment structure -
 *   standards_governance_researchers: Analytical observer
 *   (analytical/analytical) — audits participation and outcome data
 *
 * KEY AGENTS:
 *   - iesg_iab_stewardship: agenda_setter (institutional/mobile) — administers the standards process; authority rests on community consent and demonstrated judgment; collects legitimacy and mission satisfaction, not material rent
 *   - large_vendor_implementers: payer with secondary_role beneficiary (institutional/mobile) — deploys engineers into working groups, discloses patents under RF/RAND commitments, builds to consensus specs; receives an interoperable multi-vendor market in return; can defect to consortia or de facto standards at real cost
 *   - small_implementers_open_source: beneficiary (moderate/mobile) — adopts published specifications at zero licensing cost, interoperates with far larger vendors' products, contributes labor opportunistically, free to ignore standards entirely
 *   - internet_end_users: beneficiary (powerless/constrained) — inherits whatever interoperability the process preserves; cannot influence specifications directly; bound to the protocol stack their devices and services speak
 *   - exclusive_ipr_holders: excluded (powerful/arbitrage) — hold patents they would prefer to monetize per implementation; the RF/RAND commitment structure prices their participation in influence; they can litigate outside standards contexts, pool patents, or fund captive consortia instead
 *   - standards_governance_researchers: observer (analytical/analytical) — study participation logs, IPR disclosures, and specification genealogies; publish audits of who shapes consensus; no stake in outcomes beyond evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__commons_stewardship_reading, 0.14).
domain_priors:suppression_score(ietf_openness_commitment__commons_stewardship_reading, 0.1).
domain_priors:theater_ratio(ietf_openness_commitment__commons_stewardship_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__commons_stewardship_reading, rope).
narrative_ontology:human_readable(ietf_openness_commitment__commons_stewardship_reading, "IETF Openness Commitment — Commons Stewardship Reading (Open Standards as Public Infrastructure)").
narrative_ontology:topic_domain(ietf_openness_commitment__commons_stewardship_reading, "technology governance/internet standards/institutional economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__commons_stewardship_reading, 'c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b').
narrative_ontology:cs_kernel_codification('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', formalized).
narrative_ontology:cs_authority_grounding('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', practice).
narrative_ontology:cs_interpretation_layer_present('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b').
narrative_ontology:cs_reading_relation('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_reading_relation('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', ietf_openness_commitment__legitimacy_erosion_reading, coexists_with).
narrative_ontology:cs_axiom('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', foundational, interoperability_as_public_infrastructure).
narrative_ontology:cs_axiom_status(interoperability_as_public_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', interoperability_as_public_infrastructure, instrumental).
narrative_ontology:cs_axiom('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', foundational, equal_constraint_across_implementer_scale).
narrative_ontology:cs_axiom_status(equal_constraint_across_implementer_scale, holdable).
narrative_ontology:cs_axiom_grounding('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', equal_constraint_across_implementer_scale, conventional).
narrative_ontology:cs_reference_frame('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', open_commons_equal_access_regime).
narrative_ontology:cs_drift_state('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', contemporary_corporate_participation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c76f78f1-4009-4ac8-b5eb-3f49fc70bc6b', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, small_implementers_open_source).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, internet_end_users).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, large_vendor_implementers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, large_vendor_implementers).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, rough_consensus_running_code_credo).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, open_interop_network_benefit_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer leadership bodies that administer the working-group process, approve specifications for publication, enforce IPR disclosure rules, and guard the rough-consensus procedure. Their authority rests on community consent and demonstrated technical judgment rather than ownership; members rotate off routinely, and stepping down is always available. They collect legitimacy and mission satisfaction, not material rent.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, iesg_iab_stewardship, agenda_setter,
    institutional, generational, mobile, global).

% Corporations that deploy engineers into working groups, disclose relevant patents under royalty-free or RAND licensing commitments, and build products to consensus specifications rather than proprietary extensions. They bear real process costs — staff time, disclosure exposure, conformance discipline — and receive in return an interoperable multi-vendor market far larger than any proprietary stack they could sustain alone. Exit is real but costly: promoting a de facto standard or moving work to a captive consortium forfeits the shared installed base.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, large_vendor_implementers, payer,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__commons_stewardship_reading, large_vendor_implementers, beneficiary).

% Independent developers, startups, and open-source projects that adopt published specifications at zero licensing cost and interoperate with products from the largest vendors. They contribute patch-level review and code opportunistically but bear little of the process burden; ignoring the standards entirely is always available and carries no penalty.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, small_implementers_open_source, beneficiary,
    moderate, biographical, mobile, global).

% Users who inherit whatever interoperability the process preserves: devices, applications, and networks that speak common protocols regardless of vendor. They cannot influence specifications directly and are bound to the protocol stack their equipment and services speak, but they pay nothing for the coordination the constraint provides.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, internet_end_users, beneficiary,
    powerless, generational, constrained, global).

% Patent holders who would prefer to monetize essential claims per implementation. The licensing-commitment structure prices their participation in influence: contributing technology means accepting RF/RAND terms, so they tend to stay out, litigate outside standards contexts, aggregate into patent pools, or fund captive consortia where royalty-bearing terms survive. They would object that 'open' embeds one particular IP settlement.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, exclusive_ipr_holders, excluded,
    powerful, biographical, arbitrage, global).

% Academic and policy analysts who study participation logs, IPR disclosures, and specification genealogies, publishing audits of who shapes consensus and whether outcomes track input asymmetry. They hold no stake in any specification's fate beyond the evidence.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, standards_governance_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__commons_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__commons_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the protocol-fragmentation collective-action problem: without a common open process, rival proprietary protocols fragment the network and every implementer duplicates compatibility work. The openness commitment coordinates thousands of independent implementers onto shared, freely accessible specifications, preserving universal interoperability.
% TRANSFER_FUNCTION: Moves little material value. It moves participant labor and attention into the common specification pool (from contributing engineers to the published RFC corpus) and imposes conformance obligations on implementers (build to the published spec rather than private extensions); in exchange it distributes specification access freely and equally to all comers.
% ABSENT_VOICES: Exclusive IPR holders who would prefer royalty-bearing standardization are structurally cooled out by the RF/RAND commitment structure — they would argue that 'open' hard-codes one intellectual-property settlement and excludes others. Implementers in low-bandwidth regions were historically underrepresented in face-to-face meetings (partially mitigated by virtual participation), and would object that consensus reflects who can afford to show up.
% DISAPPEARANCE_RATIONALE: If the openness commitment vanished overnight, protocol fragmentation would resume: implementers would retreat toward proprietary or licensed stacks, cross-vendor interoperability at the application layer would splinter, and the small-implementer and end-user seats would lose subsidized access to the coordination good. The unified interoperable network that the arrangement preserves would reorganize into competing walled protocol families.
% FOUNDING_PROBLEM: Coordinating multi-vendor, interoperable internet protocol development without central ownership, royalty barriers, or fragmentation — posed acutely against the backdrop of proprietary stack wars (SNA, DECnet, OSI-vs-TCP/IP) and later by patent claims threatening to lock standardized technology behind licensing walls.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the business and technical history of standards wars (documented fragmentation and lock-in episodes preceding and parallel to the IETF) attests the founding problem was real, and its recurrence — every proprietary-extension push, every walled-garden interop fight in adjacent layers (messaging, IoT, AI systems) — attests that it remains live. Academic standards-governance research and the repeated failure of proprietary-lock-in strategies supply external testimony; no attestation relies solely on the IETF's own account.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__commons_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__commons_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__commons_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__commons_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__commons_stewardship_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__commons_stewardship_reading_tests).
:- end_tests(ietf_openness_commitment__commons_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics authored independently. The rope claim reflects this reading's structural assessment: a genuine collective-action solution (protocol fragmentation) with voluntary participation, no identifiable victim class, and costs sitting near the information_standard coordination floor. Extractiveness 0.14: real but modest — participant time, disclosure obligations, conformance discipline — attributed to coordination overhead rather than rent; global scope makes verification harder and the engine scales effective extraction upward modestly, which the low base absorbs. Suppression 0.10: participation is voluntary, exit is real (anyone may ignore, fork, or defect to consortia), and enforcement is procedural rather than coercive; suppression is authored as a raw structural property and is NOT scaled by power or scope. Theater 0.15: the process demonstrably produces deployed specifications; ritual accretes slowly with organizational scale. Accessibility_collapse 0.20: alternatives persist and are used — consortia (W3C, OASIS), de facto standards, proprietary protocols — so understanding the constraint does not collapse exits. Resistance 0.28: episodic and real — the 2002–2004 IPR crisis, recurring proprietary-extension pushes — but not sustained opposition. All three measurement series share one grid ({0,8,16,24,32,40}). Suppression_requirement is authored because the story specifically traces an enforcement-capacity episode: the IPR-crisis hardening (patent claims threatening the royalty-free baseline forced active disclosure enforcement, peaking at t=16), followed by decay to a stable plateau once RFC 3667/3967/8179 clarified the licensing terms — a bounded episode, not a ratchet.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical artifacts. From the steward seat the arrangement is a tended commons whose costs are stewardship labor; from the large-vendor seat it is a roughly symmetric trade — disclosure and conformance costs paid for market-wide interoperability — and their mobility (consortia, de facto paths) keeps effective extraction low; from the small-implementer seat it approaches pure subsidy (specification access without process burden); from the excluded IPR-holder seat the same licensing commitments operate as a barrier pricing them out of influence. Same nominal institution, four differently experienced constraints; the engine derives this divergence from the declared roles, power atoms, and exit options, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: small_implementers_open_source and internet_end_users (near-pure beneficiaries, d near 0.0 — the constraint subsidizes them), and large_vendor_implementers (declared beneficiary with genuine cost-bearing; their derived d sits nearer symmetric than the pure beneficiaries because they pay disclosure and conformance costs at scale, reflected in their secondary payer role). No victims are declared — by design of this reading, which holds costs to be diffuse coordination overhead rather than asymmetric extraction; no stakeholder carries payer as a PRIMARY role. Effective extraction is amplified for targets and damped for beneficiaries; with no declared targets and global scope adding only modest amplification, chi stays near the floor across seats. Directionality overrides were considered for the large-vendor seat and rejected: the beneficiary declaration plus their mobile exit options already yield a defensible d, and the override mechanism keys on power atoms rather than agent names, so differentiating seats that share an atom is better left to the structural derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating multi-vendor interoperable protocol development without central ownership or royalty barriers — remains live: every new technology layer (IoT, AI-system interoperability) re-poses it, so no mandate has outlived its function and mandatrophy_resolved is not declared. The rope classification guards against the snare misread: reading routine coordination friction (meeting load, disclosure paperwork, conformance discipline) as extraction would justify dismantling a working commons whose removal cost is prohibitive and whose gains are irrecoverably diffuse. The omega set guards against the opposite error — rope complacency: if participation asymmetry converts into outcome bias (omega participation_asymmetry_outcome_neutrality), the same artifacts instantiate the capture_substrate_reading with a structural beneficiary class, and the classification must move. The receipt surface (diffuse gains, prohibitive fixing cost) superficially matches the piton cell; the disambiguator is functional vitality — theater_ratio 0.15 and a live founding problem mark a working commons, not atrophied performance maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the commons_stewardship_reading of the ietf_openness_commitment kernel; would instantiating capture_substrate_reading or legitimacy_erosion_reading instead change the constraint''s epsilon, beneficiary structure, and computed type?',
    'Comparative authoring of the sibling files against the same process artifacts (RFC 2026/8179 texts, working-group participation logs, IPR disclosure records); divergence in authored epsilon and victim sets locates where the readings disagree.',
    'The capture_substrate_reading would declare resource_rich_implementers as a structural beneficiary class and author substantially higher epsilon (tangled_rope/snare territory); the legitimacy_erosion_reading would relocate the constraint to the rough-consensus mechanism itself with elevated suppression and fragility metrics. This file''s low epsilon is valid only under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one of three readings of the IETF openness kernel.').

omega_variable(
    participation_asymmetry_outcome_neutrality,
    'Does measured concentration of working-group participation among large-vendor employees translate into specification outcomes biased toward those vendors, or do rough-consensus safeguards neutralize input asymmetry?',
    'Outcome audits comparing published specification content against pre-process vendor positions; case studies of contests where concentrated interests were defeated (failed proprietary-extension pushes, rejected vendor-favoring requirements).',
    'If outcomes track resource advantage, this reading''s low epsilon and no-structural-beneficiary-class claim fail and the capture_substrate_reading becomes the better instantiation of the kernel; if outcomes are neutral, the commons stewardship reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_asymmetry_outcome_neutrality, empirical, 'Whether input asymmetry converts into outcome bias in the standards process.').

omega_variable(
    voluntary_compliance_durability,
    'Does the regime''s low suppression depend on a voluntary-participation culture that could erode, requiring escalating enforcement and shifting the constraint toward enforced-extraction territory?',
    'Track enforcement incidents, IPR-disclosure evasion, and fork/de-facto-standard defections across the coming decade; compare against the t=16 IPR-crisis episode as the baseline enforcement burst.',
    'A rising enforcement requirement would lift suppression above the rope band and push computed classification toward tangled_rope; stable voluntariness confirms the rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_compliance_durability, empirical, 'Durability of the low-coercion equilibrium underlying the openness regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__commons_stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_openness_commons_tr_t0, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t0, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t8, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t8, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t16, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t16, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t24, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t24, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t32, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 32, 0.13).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t32, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t40, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(ietf_openness_commons_be_t0, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t0, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t8, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t8, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t16, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t16, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t24, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t24, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t32, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t32, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t40, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(ietf_openness_commons_su_t0, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(ietf_openness_commons_su_t0, observed).
narrative_ontology:measurement(ietf_openness_commons_su_t8, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 8, 0.07).
narrative_ontology:measurement_basis(ietf_openness_commons_su_t8, observed).
narrative_ontology:measurement(ietf_openness_commons_su_t16, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 16, 0.12).
narrative_ontology:measurement_basis(ietf_openness_commons_su_t16, observed).
narrative_ontology:measurement(ietf_openness_commons_su_t24, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 24, 0.11).
narrative_ontology:measurement_basis(ietf_openness_commons_su_t24, observed).
narrative_ontology:measurement(ietf_openness_commons_su_t32, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 32, 0.1).
narrative_ontology:measurement_basis(ietf_openness_commons_su_t32, observed).
narrative_ontology:measurement(ietf_openness_commons_su_t40, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement_basis(ietf_openness_commons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__commons_stewardship_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'IETF openness' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — three readings of one kernel. This file instantiates the commons_stewardship_reading (epsilon ~0.14, no structural beneficiary class, rope). The capture_substrate_reading instantiates the same artifacts as a gatekeeping substrate (substantially higher epsilon, resource-rich beneficiary class); the legitimacy_erosion_reading relocates the constraint to the consensus mechanism itself (elevated suppression, fragility metrics). The commons reading is the upstream aspirational frame from which the two critical readings depart; each file links the others via affects_constraints so contamination and foreclosure analysis can traverse the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
