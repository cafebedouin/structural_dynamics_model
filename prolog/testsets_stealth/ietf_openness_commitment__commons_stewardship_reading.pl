% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__commons_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: ietf_openness_commitment__commons_stewardship_reading
 *   human_readable: IETF Openness Commitment as Public Infrastructure (Commons Stewardship Reading)
 *   domain: technology governance/internet standards/institutional economics
 *
 * SUMMARY:
 *   The IETF's openness commitment — specifications developed in open working
 *   groups, adopted by rough consensus, published royalty-free, and
 *   implementable by anyone — operates as public infrastructure for the
 *   internet's multi-vendor fabric. Under the commons-stewardship reading
 *   instantiated here, the arrangement's defining structural fact is
 *   symmetry: the same published documents bind the largest platform vendor
 *   and the smallest startup to interoperate, no seat collects a toll on
 *   implementation, and exit remains open — any participant may stop
 *   attending, and any vendor may ship proprietary layers outside the
 *   standard. The arrangement's residual asymmetries are participation costs
 *   and agenda influence, and under this reading they are modest. Family
 *   note: this story is one of three readings of the ietf_openness_commitment
 *   kernel; the sibling stories (capture_substrate_reading,
 *   legitimacy_erosion_reading) are separate constraints with their own
 *   epsilon values and beneficiary structures, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - working_group_participants: agenda-setting contributors ([moderate]/[mobile]) — draft, debate, and refine specifications; consensus of those present is their only sanction
 *   - iesg_iab_stewards: procedural administrators ([institutional]/[constrained]) — approve standards, hold the royalty-free IPR commitments in trust, hear appeals; cannot dictate content against sustained objection
 *   - large_platform_vendors: contributing beneficiaries ([powerful]/[mobile]) — fund participation, gain guaranteed interop, accept the largest absolute compliance surface
 *   - small_implementers_and_startups: unencumbered beneficiaries ([moderate]/[mobile]) — adopt specifications at near-zero cost; the same documents obligate incumbents to interoperate with them
 *   - internet_end_users: diffuse beneficiaries ([powerless]/[mobile]) — receive interoperability as a background property of the network; no seat in the process
 *   - institutional_economists: analytical observers ([analytical]/[analytical]) — study the arrangement as a case in commons governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__commons_stewardship_reading, 0.13).
domain_priors:suppression_score(ietf_openness_commitment__commons_stewardship_reading, 0.1).
domain_priors:theater_ratio(ietf_openness_commitment__commons_stewardship_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, extractiveness, 0.13).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__commons_stewardship_reading, rope).
narrative_ontology:human_readable(ietf_openness_commitment__commons_stewardship_reading, "IETF Openness Commitment as Public Infrastructure (Commons Stewardship Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__commons_stewardship_reading, "technology governance/internet standards/institutional economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__commons_stewardship_reading, 'e6f3d9b0-27fb-49f9-b905-a38c3f915e6d').
narrative_ontology:cs_kernel_codification('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', formalized).
narrative_ontology:cs_authority_grounding('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', practice).
narrative_ontology:cs_interpretation_layer_present('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d').
narrative_ontology:cs_reading_relation('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', ietf_openness_commitment__legitimacy_erosion_reading, coexists_with).
narrative_ontology:cs_axiom('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', foundational, universal_royalty_free_specification_access).
narrative_ontology:cs_axiom_status(universal_royalty_free_specification_access, holdable).
narrative_ontology:cs_axiom_grounding('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', universal_royalty_free_specification_access, conventional).
narrative_ontology:cs_axiom('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', foundational, rough_consensus_capture_resistance).
narrative_ontology:cs_axiom_status(rough_consensus_capture_resistance, holdable).
narrative_ontology:cs_axiom_grounding('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', rough_consensus_capture_resistance, instrumental).
narrative_ontology:cs_reference_frame('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', vendor_neutral_open_commons).
narrative_ontology:cs_drift_state('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', contemporary_commercialized_internet, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e6f3d9b0-27fb-49f9-b905-a38c3f915e6d', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, large_platform_vendors).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, small_implementers_and_startups).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, internet_end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, large_platform_vendors).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, rough_consensus_running_code).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, royalty_free_ipr_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engineers, researchers, and company delegates who draft, debate, and revise protocol specifications in working groups. Decisions are made by rough consensus of those present; anyone may join a mailing list or meeting, and anyone may stop participating without penalty beyond losing influence over the documents.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, working_group_participants, agenda_setter,
    moderate, biographical, mobile, global).

% The steering group, architecture board, and administrative LLC that run the process: they approve specifications, shepherd last calls, hold the royalty-free intellectual-property commitments in trust, and hear appeals. Their procedural authority lasts only while the contributor community keeps consenting to it; they cannot impose technical content over sustained objection.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, iesg_iab_stewards, agenda_setter,
    institutional, generational, constrained, global).

% Large technology companies that send funded engineers into working groups and ship implementations at scale. They gain guaranteed interoperability with competitors and contribute staff time in return; they also accept that their own products must interoperate rather than differentiate through incompatible extensions, though they remain free to ship proprietary layers outside the published specifications.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, large_platform_vendors, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__commons_stewardship_reading, large_platform_vendors, payer).

% Small firms and independent developers who take the published documents and build products on them without negotiating access, paying license fees, or seeking permission. The same documents obligate much larger incumbents to interoperate with what they build.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, small_implementers_and_startups, beneficiary,
    moderate, biographical, mobile, global).

% People and organizations who use email, web, and messaging services. They never chose the underlying specifications and have no seat in the process; they experience the arrangement as the ability to switch providers and mix vendors without losing connectivity.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, internet_end_users, beneficiary,
    powerless, immediate, mobile, global).

% Researchers who study the arrangement as a case in commons governance and institutional design, publishing analyses of its decision procedures, participation composition, and durability. They collect nothing from its operation and bear none of its costs.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, institutional_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__commons_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__commons_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of multi-vendor interoperability: without a shared specification, every pair of systems needs bespoke integration. Open working groups produce one documented protocol that independent implementations meet by construction, and network effects reward adoption without any party collecting a toll on implementation.
% TRANSFER_FUNCTION: Moves specification labor — engineering time, review attention, editorial work — from participants into a common pool of royalty-free public documents. It moves no money and grants no exclusive rights; the only price charged is that one's own implementations must interoperate with everyone else's.
% ABSENT_VOICES: Vendors whose revenue depends on proprietary lock-in have voice available whenever they attend, but their preferred outcome finds no procedural vehicle and they rarely sustain presence. Structurally absent seats: end users, who have no channel and rely on implementer competition to transmit their interests, and future implementers not yet in the market, who depend on the royalty-free commitment holding after they arrive.
% DISAPPEARANCE_RATIONALE: If the openness commitment vanished overnight — specifications became proprietary or license-encumbered — the internet's multi-vendor fabric would fragment over years: implementers would negotiate bilateral access, small players would be priced out of markets they currently enter by downloading a document, and walled-garden ecosystems would re-emerge around whichever vendors controlled the specifications.
% FOUNDING_PROBLEM: Late-1970s and 1980s network fragmentation: incompatible vendor architectures and proprietary protocol suites made internetworking between equipment from different makers impractical. The founding solution was a neutral specification process no single vendor controlled, with free intellectual-property terms so no implementer could be legally blocked.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the documented pre-TCP/IP protocol wars among competing vendor architectures, recorded in contemporaneous networking literature and funding-agency program records; academic histories of the OSI-versus-Internet standards contest, written by researchers with no stake in IETF self-description; and the testimony of vendors that originally resisted open interconnection and later attested its necessity after joining the process.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__commons_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__commons_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__commons_stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__commons_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__commons_stewardship_reading, 0.13, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.13 at interval end) because the arrangement charges no toll: specifications are royalty-free, and the only price is accepting that one's own implementations must interoperate. The slow upward drift (0.05 to 0.13) tracks rising participation costs — travel, employer-sponsored time, English-language document culture — which tilt voice toward the resourced without excluding anyone. Suppression is low (0.10) and static: adoption is voluntary, proprietary alternatives remain lawful and are periodically shipped, and the arrangement's pull is network effect rather than enforcement machinery; accordingly no suppression_requirement series is authored (static enforcement picture — the scalar carries it). Theater is low (0.16): process ritual exists (ceremony, humor, tradition) but the output is functioning specifications. Accessibility_collapse (0.50) is mid-range: once a protocol is understood, incompatible alternatives remain buildable but forfeit the network, so alternatives collapse only as far as interdependence reaches. Resistance (0.20) reflects occasional vendor pushback and embrace-and-extend attempts, routinely absorbed by the community. Both tracked series share one six-point grid (1986-2026). Receipt surface: no seat captures the arrangement's gains — value accrues diffusely to implementers and users, affirmatively checked across all six seats (gain_flow: diffuse); altering the royalty-free commitment would require overturning broad consensus and destroying the trust asset implementers rely on, a cost exceeding any private benefit (fixing_cost: prohibitive).
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the steward seat the arrangement is a trusteeship whose authority exists only while the community consents. From the large-vendor seat it is a valuable utility that occasionally chafes — the same documents that guarantee interoperation forbid ratifying proprietary advantage. From the small-implementer seat it is a lifeline: market access without negotiation or permission. From the user seat it is invisible infrastructure experienced only as the ability to switch providers. The engine computes these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party sits on the beneficiary side of the ledger — there is no victim class under this reading, which is the reading's defining structural delta. Working-group participants and small implementers derive near-full-beneficiary directionality from the declarations alone. Two overrides correct derivations the structural data cannot see: large_platform_vendors would derive as near-pure beneficiaries from the beneficiary declaration, but they surrender the largest option value — the lock-in rent a proprietary stack would earn — so their true position sits nearer symmetric (d=0.30); the arrangement's principal cost falls on whoever had the most to gain from closure. The institutional stewards have no declaration-derived position at all; they subsidize the arrangement with administrative labor while collecting no rents (d=0.20).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vendor-fragmented networks that could not interconnect — recurs at every new layer (messaging encryption, IoT, AI-system interoperation), so the mandate has not outlived its function and no sunset applies. The classification guards both mislabeling directions: against pure-extraction labeling (there is no victim class to name, and exit is open on every seat), and against complacent certification (the omegas hold open the possibility that resource advantage or IPR erosion could convert the commons into something extractive — developments that would belong to the sibling readings' stories, not this one).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the commons_stewardship_reading of the ietf_openness_commitment kernel; which of the three readings correctly characterizes the operative constraint?',
    'Cross-reading comparison of the three sibling stories'' epsilon values, beneficiary structures, and computed types, adjudicated by compositional evidence: authorship-concentration data, IPR litigation history, and participation composition studies.',
    'Under the capture_substrate_reading, epsilon rises sharply and resource-advantaged vendors become a structural beneficiary class; under the legitimacy_erosion_reading, suppression and theater_ratio rise as safeguards prove theatrical. This story''s low-epsilon profile holds only if the stewardship claims survive that evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: this constraint is one of three readings of the openness-commitment kernel, and the classification is reading-indexed.').

omega_variable(
    resource_advantage_agenda_control,
    'Does resource advantage translate into disproportionate agenda control despite formally open procedures?',
    'Working-group composition and specification-authorship concentration studies over time; comparison of published outcomes against the documented positions of the best-funded participants.',
    'Strong concentration would shift the effective directionality of large_platform_vendors upward and lend the capture_substrate_reading its epsilon; diffuse contribution across many employers would confirm this reading''s equal-constraint claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_advantage_agenda_control, empirical, 'Whether open procedures neutralize resource asymmetry in agenda formation.').

omega_variable(
    royalty_free_ipr_durability,
    'Will the royalty-free intellectual-property commitment hold as standards become commercially central and implementation-adjacent patents proliferate?',
    'Track standards-essential-patent-adjacent litigation and IETF Trust policy enforcement across successor decades; observe whether conformant implementers ever encounter licensing walls.',
    'Erosion would convert the commons into licensed infrastructure, raising epsilon and creating a victim class of blocked implementers, moving the operative constraint toward the sibling readings'' territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royalty_free_ipr_durability, empirical, 'Durability of the free-IPR term that makes the specifications a commons.').

omega_variable(
    rough_consensus_legitimacy_at_scale,
    'Does rough consensus retain legitimacy and capture-resistance as participation scales and corporate affiliation comes to dominate attendance?',
    'Appeals history, disputed-consensus rates, and independent audits comparing decision outcomes with expressed dissent in working groups.',
    'If the safeguards prove theatrical, theater_ratio and suppression rise and the legitimacy_erosion_reading''s characterization gains force; this reading''s classification presupposes functioning safeguards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rough_consensus_legitimacy_at_scale, conceptual, 'Whether the consensus mechanism itself withstands organized influence at scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__commons_stewardship_reading, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_openness_commons_tr_t1986, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 1986, 0.08).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t1986, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t1996, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 1996, 0.1).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t1996, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t2006, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 2006, 0.12).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t2006, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t2016, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 2016, 0.14).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t2016, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t2021, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 2021, 0.15).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t2021, observed).
narrative_ontology:measurement(ietf_openness_commons_tr_t2026, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 2026, 0.16).
narrative_ontology:measurement_basis(ietf_openness_commons_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ietf_openness_commons_be_t1986, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 1986, 0.05).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t1986, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t1996, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 1996, 0.07).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t1996, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t2006, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 2006, 0.09).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t2006, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t2016, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 2016, 0.11).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t2016, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t2021, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 2021, 0.12).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t2021, observed).
narrative_ontology:measurement(ietf_openness_commons_be_t2026, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 2026, 0.13).
narrative_ontology:measurement_basis(ietf_openness_commons_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ietf_openness_commitment__commons_stewardship_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__commons_stewardship_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'IETF openness' into three structurally distinct claims, per the epsilon-invariance principle: (1) this story — the arrangement as public infrastructure constraining all implementers equally (low epsilon, no beneficiary class); (2) capture_substrate_reading — the process as coordination substrate where resource advantage translates into encoded gatekeeping (higher epsilon, resource-advantaged beneficiary class); (3) legitimacy_erosion_reading — rough consensus itself as contested and vulnerable to organized capture (higher suppression and theater). Each member carries its own epsilon, stakeholders, and classification; the family is linked through network.affects_constraints. The stewardship reading is the baseline characterization the other two contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__commons_stewardship_reading, powerful, 0.3).
constraint_indexing:directionality_override(ietf_openness_commitment__commons_stewardship_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
