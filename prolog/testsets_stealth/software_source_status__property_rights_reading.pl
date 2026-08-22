% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Software Source Status — Property Rights Reading (Proprietary Licensing Regime)
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the software_source_status kernel:
 *   the property-rights reading, under which source code is a proprietary
 *   asset, licensing restrictions are legitimate exercises of ownership, and
 *   users are consumers holding contractual rights only. The standing
 *   arrangement under contest — and therefore the ε referent — is the
 *   existing proprietary-licensing regime assessed BY THIS READING'S OWN
 *   LIGHTS: license payments are substantially fair exchange for funded
 *   development, maintenance, and liability-bearing, with residual concern
 *   where pricing decouples from cost. Sibling readings (freedom-imperative,
 *   pragmatic-development, utilitarian-hybrid) are separate constraint files
 *   with their own ε, beneficiary/victim structures, and classifications;
 *   nothing here averages over them. KEY AGENTS (by structural relationship):
 *   - proprietary_software_publishers: agenda_setter
 *   (institutional/arbitrage) — owns the code, sets terms, collects fees -
 *   commercial_licensees: primary payer (powerful/constrained) — buys
 *   enterprise rights inside vendor-written frameworks -
 *   individual_end_users: payer with incidental beneficiary position
 *   (moderate/constrained) — consumes under take-it-or-leave-it EULAs -
 *   independent_developers: excluded (moderate/mobile) — barred from
 *   inspecting or extending the code; exits by building open alternatives -
 *   security_researchers: excluded (organized/constrained) — inspection
 *   legally exposed under anti-circumvention rules -
 *   ip_courts_and_legislatures: analytical observer
 *   (institutional/analytical) — determines which exercises of the right the
 *   framework backs
 *
 * KEY AGENTS:
 *   - proprietary_software_publishers: agenda_setter (institutional/arbitrage) — owns code, drafts EULA terms, collects license and subscription revenue, can restructure the model at will
 *   - commercial_licensees: primary payer (powerful/constrained) — enterprises negotiating volume discounts within vendor-set frameworks, facing high switching costs
 *   - individual_end_users: payer / secondary beneficiary (moderate/constrained) — consume under click-through terms, receive updates and support, may not modify or redistribute
 *   - independent_developers: excluded (moderate/mobile) — locked out of proprietary codebases, respond by building open-source substitutes
 *   - security_researchers: excluded (organized/constrained) — vulnerability research exposed to anti-circumvention liability
 *   - ip_courts_and_legislatures: observer (institutional/analytical) — interpret copyright doctrine and legislate exceptions bounding the property right
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.32).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.42).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software Source Status — Property Rights Reading (Proprietary Licensing Regime)").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '9b03f549-ccf4-47bc-8d74-bedf7e4318cc').
narrative_ontology:cs_kernel_codification('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', distributed).
narrative_ontology:cs_authority_grounding('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', lineage).
narrative_ontology:cs_interpretation_layer_present('9b03f549-ccf4-47bc-8d74-bedf7e4318cc').
narrative_ontology:cs_reading_relation('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', foundational, creator_ownership_entitles_access_control).
narrative_ontology:cs_axiom_status(creator_ownership_entitles_access_control, holdable).
narrative_ontology:cs_axiom_grounding('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', creator_ownership_entitles_access_control, deontological).
narrative_ontology:cs_axiom('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', secondary, license_terms_bind_as_contract).
narrative_ontology:cs_axiom_status(license_terms_bind_as_contract, holdable).
narrative_ontology:cs_axiom_grounding('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', license_terms_bind_as_contract, conventional).
narrative_ontology:cs_reference_frame('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', code_as_copyrighted_work).
narrative_ontology:cs_drift_state('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', contemporary_open_source_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9b03f549-ccf4-47bc-8d74-bedf7e4318cc', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, individual_end_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, commercial_licensees).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, individual_end_users).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, copyright_incentive_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, lockean_labor_desert).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and own source code, sell access through perpetual licenses and subscriptions, draft the EULA terms under which every copy ships, and enforce those terms through contract audits, anti-circumvention suits, and technical protection measures. Because they control the asset, they can also restructure the model at will — shifting to SaaS rental, dual-licensing, or selectively opening components — which is an exit option no other seat holds.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Enterprises that buy enterprise agreements and per-seat licenses to run mission-critical software. They negotiate discounts and audit exposure from a position of volume, but within frameworks the vendor wrote; migrating off an entrenched platform means retraining staff, rewriting integrations, and re-certifying compliance, so switching is possible only slowly and at high cost.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, commercial_licensees, payer,
    powerful, biographical, constrained, global).

% Purchase or subscribe to software under click-through terms they did not negotiate, receiving working code, updates, and support in exchange. They may not inspect, modify, or redistribute what they run. Where their tools, files, and skills are bound to one vendor's ecosystem, leaving means abandoning accumulated work and familiarity.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, individual_end_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, individual_end_users, beneficiary).

% Developers who would inspect, interoperate with, or extend proprietary codebases but face closed APIs, confidentiality terms, and anti-circumvention provisions. Their realistic path is outside the arrangement entirely: building open-source alternatives they control, which they can do freely because writing new code is not restricted.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_developers, excluded,
    moderate, biographical, mobile, global).

% Researchers who probe shipped binaries for vulnerabilities and disclose findings. Anti-circumvention rules and license terms restrict the inspection techniques they may lawfully use, exposing them to legal risk that vendors can invoke selectively; coordinated professional norms and disclosure frameworks partially protect them.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, security_researchers, excluded,
    organized, biographical, constrained, global).

% Courts interpret whether license terms, anti-circumvention claims, and interface copyrights hold; legislatures carve exceptions such as fair use, interoperability exemptions, and repair rights. They neither buy nor sell software but determine which exercises of the property right the framework will back.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, ip_courts_and_legislatures, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_publishers).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Proprietary licensing solves the funding-and-maintenance problem for commercial software: upfront license and subscription revenue finances development, and a single accountable vendor delivers updates, security patches, compatibility guarantees, and support that scattered unpaid production does not reliably provide.
% TRANSFER_FUNCTION: Moves license fees, subscriptions, and audit settlements from commercial licensees and end users to software publishers, in exchange for contractual usage rights; the code itself never changes hands.
% ABSENT_VOICES: Users and independent developers who would modify, audit, or redistribute the code have no seat where license terms are set — EULAs arrive on take-it-or-leave-it terms, and interoperability-seekers engage only at the vendor's discretion. They exist outside the arrangement, building open alternatives, rather than inside it.
% DISAPPEARANCE_RATIONALE: If proprietary licensing vanished overnight, the primary revenue mechanism of commercially funded software production would disappear; vendor update pipelines, enterprise procurement, certification regimes, and the support economy built on licensed software would all reorganize around alternative funding — service contracts, patronage, public provision. The rearrangement is large, which is exactly why this reading treats the arrangement as load-bearing coordination rather than dispensable rent.
% FOUNDING_PROBLEM: Once software decoupled from hardware, freely circulating source code meant firms could not recoup development cost: anyone could copy the finished product without sharing the expense of producing it. The arrangement was built to let creators fund continued development by controlling copying and charging for usage.
% FOUNDING_PROBLEM_CORROBORATION: Industry history attests from outside the beneficiary set: the 1969 IBM unbundling decision created the independent software-products market precisely because per-copy licensing made software a separately sellable good, and economic histories of the industry document the funding problem as the arrangement's origin. Open-source economists analyzing public-goods underproduction concede the funding problem is real even while disputing this reading's moral conclusions; no serious participant claims the original problem never existed.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).
:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.32) because this reading assesses the arrangement by its own lights: license revenue tracks real development, maintenance, and support costs for most of the catalog, with residual extraction where dominant products price above any cost basis. Suppression (0.42) records that enforcement is real — contract audits, anti-circumvention suits, technical protection measures — but bounded, because writing and distributing open alternatives is lawful and pervasive, so the constraint suppresses access to THIS code, not to software generally. Theater ratio (0.22) is low: security-patch, warranty, and support functions are substantially performed, though marketing increasingly dresses control as 'value delivery.' Accessibility collapse (0.30) is low because the open-source alternative is fully understood, mature, and legally available — alternatives do not collapse once the constraint is understood. Resistance (0.50) reflects three decades of organized free-software advocacy, right-to-repair campaigns, and EULA litigation. The temporal series share one grid (points 0–30). The suppression series shows a documented two-phase shape: intensification through the anti-circumvention-statute era (rise to 0.44 by midpoint) followed by plateau and slight decline as vendors migrated enforcement from litigation into service architecture and technical measures, which normalize control and reduce courtroom dependence. Extractiveness creeps upward across the interval as subscription repricing concentrates on captive installed bases; theater rises gently alongside the shift from selling copies to renting access.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the publisher seat the arrangement is coordination it financed and maintains: without controlled copying there is no revenue, and without revenue no sustained professional engineering. From the commercial-licensee seat the same structure is a necessary purchase inside terms it did not write, tolerable because switching is worse. From the end-user seat it is a take-it-or-leave-it contract binding conduct the buyer was never consulted on. From the excluded seats — independent developers, security researchers — the operative fact is not the price but the wall: the code's interior is off-limits regardless of willingness to pay. The engine computes per-seat classifications from the structural data; the authored rope claim is this reading's own verdict and does not adjudicate those divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers sit nearest the beneficiary end: they collect the transfer, set the terms, and hold arbitrage-grade model mobility (SaaS conversion, dual-licensing, selective opening), so the constraint subsidizes them structurally. Commercial licensees and end users sit toward the target end: they pay the transfer and their exit is constrained by migration cost and ecosystem lock-in, with end users further out than enterprises because they lack negotiating leverage. Independent developers and security researchers are positioned at the exclusion boundary — they are kept from the object of the constraint rather than taxed by it, which the beneficiary/victim declarations alone underdescribe; their situations are carried in the stakeholder layer. Courts and legislatures observe from an analytical seat with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling creators to fund ongoing development when copies are costless to duplicate — remains live, so this is not a mandate outliving its function and no sunset applies. The classification discipline cuts both ways here: the rope claim keeps the genuine funding-coordination function visible, preventing opponents from dismissing the whole regime as pure extraction, while the independently authored metrics keep the enforced-transfer surface measurable, preventing this reading from waving away audit regimes and anti-circumvention enforcement as costless background. The live failure mode to watch is mutation rather than atrophy: as subscription models complete the shift from selling copies to renting access, the founding problem quietly changes from 'how do creators get paid' to 'how do vendors keep tenants paying,' and the constraint's type should be re-examined at that transition rather than carried forward on inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the software_source_status kernel — how would instantiating a sibling reading (freedom_imperative, pragmatic_development, utilitarian_hybrid) change the beneficiary/victim structure and ε over the same standing arrangement?',
    'Generate the sibling readings as separate constraint files and compare computed per-seat classifications; the cross-reading deltas locate where the disagreement is structural rather than rhetorical.',
    'Under the freedom-imperative reading, end users become rights-holders rather than consumers, victim groups appear where this reading declares none, and ε over the identical arrangement rises sharply; under the pragmatic reading the valuation axis shifts from legitimacy to development-method performance. This file''s low authored ε is a property of THIS reading, not of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed nature of ε and role structure over the shared kernel.').

omega_variable(
    property_frame_naturalness,
    'Is the treatment of source code as ownable property a natural extension of physical-property intuition, or a constructed statutory grant that legislatures could withdraw?',
    'Legal-historical analysis of copyright''s extension to software (CONTU proceedings, subsequent statutes, case-law divergence across jurisdictions) and comparison with jurisdictions that declined software patent/copyright parity.',
    'If the frame is a constructed grant, the constraint carries no mountain-like naturality and its persistence depends wholly on ongoing legislative maintenance; publishers become holders of a revocable monopoly rather than owners exercising natural right, which changes how their beneficiary position is weighted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_frame_naturalness, conceptual, 'Natural-right versus statutory-grant status of software property.').

omega_variable(
    license_price_cost_decoupling,
    'Do license and subscription prices track the cost and risk of developing and maintaining the software, or do they decouple toward market-power rents in concentrated product categories?',
    'Compare pricing against development-cost accounting and competitive-category benchmarks; regulatory discovery in abuse-of-dominance proceedings against dominant vendors.',
    'Wide decoupling in dominant categories would raise effective extraction for the payer seats and push computed per-seat types toward tangled_rope or snare despite this reading''s modest authored ε; narrow decoupling would confirm the fair-exchange reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(license_price_cost_decoupling, empirical, 'Whether licensing transfers fair compensation or rents.').

omega_variable(
    saas_architectural_enforcement_shift,
    'Is restriction migrating from copyright law into service architecture (SaaS delivery, technical protection measures), and does that migration change the constraint''s suppression character?',
    'Track the enforcement mix across the interval''s later points: litigation and audit volume versus adoption of hosted-only delivery and hardware-locked licensing.',
    'If architecture replaces law as the binding mechanism, suppression becomes resistant to the copyright-exception toolkit (fair use, interoperability exemptions, repair rights) that constrains legal enforcement — durable suppression could rise even as courtroom enforcement falls, dating any future type transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_architectural_enforcement_shift, empirical, 'Enforcement-mode migration and its effect on suppression durability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(software_property_reading_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(software_property_reading_tr_t0, observed).
narrative_ontology:measurement(software_property_reading_tr_t6, software_source_status__property_rights_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(software_property_reading_tr_t6, observed).
narrative_ontology:measurement(software_property_reading_tr_t12, software_source_status__property_rights_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement_basis(software_property_reading_tr_t12, observed).
narrative_ontology:measurement(software_property_reading_tr_t18, software_source_status__property_rights_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement_basis(software_property_reading_tr_t18, observed).
narrative_ontology:measurement(software_property_reading_tr_t24, software_source_status__property_rights_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement_basis(software_property_reading_tr_t24, observed).
narrative_ontology:measurement(software_property_reading_tr_t30, software_source_status__property_rights_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(software_property_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(software_property_reading_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement_basis(software_property_reading_be_t0, observed).
narrative_ontology:measurement(software_property_reading_be_t6, software_source_status__property_rights_reading, base_extractiveness, 6, 0.27).
narrative_ontology:measurement_basis(software_property_reading_be_t6, observed).
narrative_ontology:measurement(software_property_reading_be_t12, software_source_status__property_rights_reading, base_extractiveness, 12, 0.29).
narrative_ontology:measurement_basis(software_property_reading_be_t12, observed).
narrative_ontology:measurement(software_property_reading_be_t18, software_source_status__property_rights_reading, base_extractiveness, 18, 0.3).
narrative_ontology:measurement_basis(software_property_reading_be_t18, observed).
narrative_ontology:measurement(software_property_reading_be_t24, software_source_status__property_rights_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(software_property_reading_be_t24, observed).
narrative_ontology:measurement(software_property_reading_be_t30, software_source_status__property_rights_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement_basis(software_property_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(software_property_reading_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(software_property_reading_su_t0, observed).
narrative_ontology:measurement(software_property_reading_su_t6, software_source_status__property_rights_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement_basis(software_property_reading_su_t6, observed).
narrative_ontology:measurement(software_property_reading_su_t12, software_source_status__property_rights_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(software_property_reading_su_t12, observed).
narrative_ontology:measurement(software_property_reading_su_t18, software_source_status__property_rights_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement_basis(software_property_reading_su_t18, observed).
narrative_ontology:measurement(software_property_reading_su_t24, software_source_status__property_rights_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement_basis(software_property_reading_su_t24, observed).
narrative_ontology:measurement(software_property_reading_su_t30, software_source_status__property_rights_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(software_property_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the status of software source code' decomposes into four structurally distinct readings of one kernel; this file instantiates the property-rights reading only. Its ε (0.32) assesses the standing proprietary-licensing arrangement by this reading's own lights; sibling files author different ε over the same referent — the freedom-imperative reading authors high extraction with users as victims, the pragmatic reading evaluates on methodological rather than moral axes, and the utilitarian-hybrid reading conditions legitimacy on aggregate welfare. Family links run through network.affects_constraints; no reading averages another, and the upstream/downstream citation traffic between them (each reading cites the others' failures as evidence) is carried by these edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
