% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive Copyright Relaxation as Corporate Moat Enabler
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   Under the corporate-moat reading of the permissive_license_text kernel,
 *   copyright relaxation — MIT/BSD/Apache-class grants allowing unrestricted
 *   commercial reuse without reciprocity — functions as the channel through
 *   which commons-produced software value drains into proprietary
 *   derivatives. Enterprises embed permissively licensed components in
 *   closed-source products and metered cloud services, book the revenue, and
 *   return a small, selective slice of engineering attention upstream. The
 *   people who triage issues, review patches, and absorb security response
 *   for globally deployed code are overwhelmingly uncompensated individuals;
 *   the grants they issued are irrevocable, so the stock of code available
 *   for uncompensated capture only accumulates. Corporate-funded foundations
 *   steward permissive defaults and lend the arrangement its neutrality
 *   framing. KEY AGENTS (by structural relationship): -
 *   proprietary_software_vendors: Primary beneficiary (powerful/arbitrage) —
 *   embed permissive code in closed products, set return-flows unilaterally -
 *   cloud_infrastructure_providers: Secondary beneficiary
 *   (powerful/arbitrage) — monetize open stacks as managed services, fork
 *   away reciprocity attempts - individual_open_source_maintainers: Primary
 *   target (powerless/identity_locked) — carry unpaid maintenance and
 *   security burden - commons_contributing_developers: Target
 *   (powerless/trapped) — accepted work ships in derivatives they cannot
 *   access - corporate_funded_oss_foundations: Agenda-setter
 *   (institutional/constrained) — stabilize permissive norms, financed by the
 *   consuming corporations - fragmented_contributor_copyright_holders:
 *   Excluded seat (moderate/trapped) — joint copyright leverage never
 *   assembled - critical_infrastructure_dependents: Dual-positioned seat
 *   (organized/constrained) — free inputs, borne outage and breach risk -
 *   licensing_law_researchers: Analytical observer (analytical/analytical)
 *   This file instantiates ONE reading of a three-reading kernel (see
 *   kernel_context and the network note); the sibling stories measure the
 *   same instrument with different epsilon by design. Epsilon here refers to
 *   the standing permissive arrangement as this reading assesses it — never
 *   to the reciprocal-licensing alternative, which this reading analyzes but
 *   does not endorse.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Primary beneficiary (powerful/arbitrage) — captures product revenue on commons-built inputs
 *   - cloud_infrastructure_providers: Secondary beneficiary (powerful/arbitrage) — captures subscription revenue on open stacks
 *   - individual_open_source_maintainers: Primary target (powerless/identity_locked) — unpaid triage, review, and security response
 *   - commons_contributing_developers: Target (powerless/trapped) — contributions ship in inaccessible derivatives
 *   - corporate_funded_oss_foundations: Agenda-setter (institutional/constrained) — administers permissive defaults under consumer funding
 *   - fragmented_contributor_copyright_holders: Excluded seat (moderate/trapped) — unassembled joint copyright leverage
 *   - critical_infrastructure_dependents: Dual seat (organized/constrained) — free code, borne outage and breach risk
 *   - licensing_law_researchers: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.65).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.32).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive Copyright Relaxation as Corporate Moat Enabler").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "economic/technological/legal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'fba23730-28a4-4cb8-b0cd-7d9257ce743f').
narrative_ontology:cs_kernel_codification('fba23730-28a4-4cb8-b0cd-7d9257ce743f', fixed_text).
narrative_ontology:cs_authority_grounding('fba23730-28a4-4cb8-b0cd-7d9257ce743f', extraction).
narrative_ontology:cs_interpretation_layer_present('fba23730-28a4-4cb8-b0cd-7d9257ce743f').
narrative_ontology:cs_reading_relation('fba23730-28a4-4cb8-b0cd-7d9257ce743f', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('fba23730-28a4-4cb8-b0cd-7d9257ce743f', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('fba23730-28a4-4cb8-b0cd-7d9257ce743f', foundational, uncompensated_proprietary_extraction_structural).
narrative_ontology:cs_axiom_status(uncompensated_proprietary_extraction_structural, holdable).
narrative_ontology:cs_axiom_grounding('fba23730-28a4-4cb8-b0cd-7d9257ce743f', uncompensated_proprietary_extraction_structural, empirically_contingent).
narrative_ontology:cs_axiom('fba23730-28a4-4cb8-b0cd-7d9257ce743f', secondary, adoption_gain_no_offset_for_unreturned_value).
narrative_ontology:cs_axiom_status(adoption_gain_no_offset_for_unreturned_value, holdable).
narrative_ontology:cs_axiom_grounding('fba23730-28a4-4cb8-b0cd-7d9257ce743f', adoption_gain_no_offset_for_unreturned_value, deontological).
narrative_ontology:cs_reference_frame('fba23730-28a4-4cb8-b0cd-7d9257ce743f', peer_exchange_reciprocal_sharing).
narrative_ontology:cs_drift_state('fba23730-28a4-4cb8-b0cd-7d9257ce743f', contemporary_cloud_extraction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fba23730-28a4-4cb8-b0cd-7d9257ce743f', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, cloud_infrastructure_providers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_open_source_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, commons_contributing_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, critical_infrastructure_dependents).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, critical_infrastructure_dependents).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, permissionless_innovation_doctrine).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, information_wants_to_be_free_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Embed permissively licensed components inside closed-source products and sell the result without publishing modifications or paying royalties. They select targets wherever the license permits, and fork or abandon projects when governance turns unfavorable. Their product roadmaps, not any contract, decide how much flows back upstream.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Operate managed services built on open stacks whose development they did not commission, charging subscriptions while contributing selectively. When an upstream project changes its terms to demand payment, they fork the last permissive release and continue; the standing option to walk keeps negotiating leverage on their side.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, cloud_infrastructure_providers, beneficiary,
    powerful, biographical, arbitrage, global).

% Steward the license policies, trademarks, and project infrastructures through which most widely deployed permissive code moves. Funded predominantly by the corporations that consume the code, they lend the permissive default its neutrality framing and keep it stable. Changing course would mean confronting the member companies that finance them.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, corporate_funded_oss_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Maintain widely depended-upon packages in spare time or under-funded roles: triaging issue queues, reviewing unsolicited patches, carrying security response for deployments they never see. Walking away means abandoning communities and reputations built over years; relicensing future versions is mostly barred because contributed copyright is scattered across hundreds of strangers. The project tends to function as part of who they are, which makes quitting feel unlike resigning a job.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_open_source_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Submit features, fixes, and documentation to shared projects expecting the work to remain common. Their accepted contributions routinely ship inside proprietary derivatives they cannot access, and their scattered copyright grants make collective relicensing impractical. Their remaining leverage is limited to withholding future labor.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, commons_contributing_developers, payer,
    powerless, biographical, trapped, global).

% Thousands of individuals jointly hold copyright over mature codebases, yet no mechanism ever assembled them to vote on license terms. Their combined ownership would be sufficient to relicense toward reciprocity, but it exists only on paper: unorganized, unrepresented, and largely unaware of the leverage they collectively hold. Their prior grants cannot be revoked.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, fragmented_contributor_copyright_holders, excluded,
    moderate, biographical, trapped, global).

% Governments, banks, hospitals, and manufacturers run essential systems on components maintained by one or two unpaid people. They receive the code free of charge and carry the outage and breach risk when maintenance fails. They can fund or staff around specific projects, but untangling deeply embedded dependencies is slow and costly, and sector-wide support efforts have repeatedly stalled.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, critical_infrastructure_dependents, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, critical_infrastructure_dependents, beneficiary).

% Track license adoption, corporate contribution asymmetries, and relicensing episodes; publish analyses of how grant terms shape value flows. They hold no enforcement or administrative role; their influence runs through litigation, policy comment, and the scholarly record.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, licensing_law_researchers, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates reuse negotiation across organizational boundaries: one standardized grant lets any party integrate, embed, and redistribute the code, so adoption decisions stop requiring case-by-case legal bargaining and interoperable ecosystems can form.
% TRANSFER_FUNCTION: Moves maintenance labor, security response, design judgment, and finished code produced by volunteers and under-funded maintainers into proprietary products and subscription services; moves revenue and pricing power to the enterprises selling them; returns no contractual compensation and only selective, discretionary engineering effort to the producing commons.
% ABSENT_VOICES: Fragmented contributor copyright holders — the thousands of individuals whose joint ownership would be decisive — have never been assembled or represented in any license-standardization venue. Dependent sectors (healthcare, government, finance) that carry the downside of maintenance failure had no seat when permissive defaults hardened. Copyleft advocates contest from outside the corporate procurement channels where defaults are effectively set.
% DISAPPEARANCE_RATIONALE: If permissive relaxation vanished overnight, closed products embedding open components would face immediate licensing renegotiation or removal, managed-service offerings built on open stacks would lose their input basis or absorb steep new costs, and corporate software R&D budgets would visibly expand to replace what the commons supplied — the modern software supply chain would reorganize around negotiated or reciprocal terms within quarters.
% FOUNDING_PROBLEM: Software sharing in the 1980s required bespoke legal negotiation for every act of reuse; permissive licenses were drafted to remove that friction so research, industry, and hobbyist communities could build on shared code without engaging lawyers each time.
% FOUNDING_PROBLEM_CORROBORATION: License historians and IP scholars independently corroborate the original friction problem and its resolution. The shifted-function reading — that the operative problem is now uncompensated maintenance burden rather than circulation friction — is corroborated from outside the benefiting parties by neutral censuses of critical-open-source funding gaps and by post-incident analyses (Heartbleed, Log4Shell) attributing failures to underfunded maintenance, not licensing friction.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the transfer is large and persistent — enterprise products and services built on volunteer-maintained code return a minority of the value consumed — tempered by genuine reciprocity pockets and by the real utility the grant creates for non-commercial users. Suppression 0.32: nothing legally coerces participation; persistence rides on irrevocable prior grants (a ratchet — yesterday's generosity forecloses tomorrow's recall), permissive-default conventions, and corporate procurement preference: structural pressure without force. Per the framework's division of labor, suppression is authored as a raw structural property and is NOT scaled by power or scope; the engine scales only extractiveness, via directionality and scope. Theater_ratio 0.40: branded open-source offices, volume-oriented contribution programs, and giving-back messaging have grown faster than funded maintenance; roughly two-fifths of visible arrangement-supporting activity is performative rather than load-bearing. Accessibility_collapse 0.35: alternatives remain fully legible and live (copyleft licenses, dual licensing, source-available terms, funding consortia) — understanding the constraint does not close exits, which caps suppression and distinguishes this from a forced structure. Resistance 0.52: repeated relicensing waves, maintainer burnout organizing, and public critiques of corporate consumption meet the arrangement continuously. Temporal series run on one shared seven-point grid (1998-2026) with both tracked metrics authored at every point; the drift is monotonic accumulation, not oscillation, so no cyclical-interval design is warranted, and a suppression_requirement series is deliberately omitted because enforcement capacity is not the dynamic being traced — the static scalar carries it.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the vendor and cloud seats the arrangement presents as frictionless input acquisition — a working coordination surface they did not have to build; from the maintainer seat the identical surface presents as enforced asymmetry — obligation without compensation, exit blocked by scattered copyright and by a self-concept built through stewardship. The identity-lock on maintainers is relational and institutional rather than ideological: the project functions as extended self and professional reputation, so resignation registers as abandonment of persons, not exit from a contract; if funding norms normalized handing projects over, that seat's exit would relax toward constrained and the effective pressure on it would fall. The agenda-setter seat experiences neutral stewardship whose financing quietly depends on the consumers of the code. Individually powerless payers retain unrealized coalition potential — joint copyright, coordinated relicensing, sector-wide funding consortiums — which the powerless atom understates; the fragmented-holder seat keeps that latent leverage visible (see the relicensing_leverage_realizability omega).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: vendors and cloud providers (declared beneficiaries, arbitrage exit) land near the beneficiary pole; maintainers (declared victim, identity_locked exit) and contributing developers (declared victim, trapped) land near the full-target pole — amplification of trapped and identity-locked targets is the engine's computation, not an authored value. Three overrides correct relationships the structural arrays cannot express: institutional -> 0.30 because the foundations appear in neither array and would otherwise take a blind power-atom fallback, while their funding dependence on the consuming corporations places them mildly on the beneficiary side of neutral; organized -> 0.55 because the infrastructure dependents are dual-positioned (payer with secondary benefit) and a victim-keyed derivation alone would overshoot their near-symmetric net position; moderate -> 0.45 because the fragmented copyright holders hold dormant principal rights with no active cost flow — neither collecting nor currently extracted against — and an undeclared moderate agent would otherwise inherit an unjustified default.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — eliminating per-negotiation copyright friction so code could circulate — is contested rather than dead: circulation friction genuinely fell and stays fallen, but the operative problem the arrangement now manages is uncompensated industrial-scale appropriation, which the founding grant terms never priced. The classification guards against both mislabels. Reading the arrangement as pure coordination erases the named victim structure that the transfer asymmetry makes unavoidable. Reading it as degraded inertia fails on the receipt surface: gains concentrate in named corporate seats rather than diffusing, so there is no cost-asymmetry-without-capturer signature. Holding the snare claim keeps the victim structure visible while the moderate suppression score keeps the coercive story honest — this arrangement binds by ratchet and convention more than by force. Mandatrophy is not declared resolved: the mandate is annexed, not expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the corporate-moat reading of the permissive_license_text kernel: is the uncompensated-extraction structure an intrinsic property of copyright relaxation, or a property visible only from this seat — and how would classification shift under the commons-coordination or copyleft-counterfactual sibling readings?',
    'Compile and compare the sibling stories from the same kernel: if commons_coordination_reading computes near the coordination-cost floor and copyleft_counterfactual_reading computes high epsilon with a shifted remedy locus, the divergence lives in the readings, not the instrument.',
    'If the moat reading is rejected, epsilon collapses toward coordination cost and the arrangement reclassifies toward hybrid coordination; if retained, the victim structure stands and remedies aim at reciprocity mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the permissive-license kernel governs classification.').

omega_variable(
    corporate_contribution_asymmetry,
    'What fraction of enterprise consumption of permissively licensed code returns to upstream projects as substantive maintenance contribution, versus what fraction is taken without return?',
    'Longitudinal commit-attribution telemetry across major repositories matched against commercial deployment indicators, conducted by neutral research groups.',
    'A high asymmetry confirms the extraction diagnosis and holds epsilon up; materially reciprocal behavior would push the arrangement toward hybrid coordination/extraction and soften victim attribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_contribution_asymmetry, empirical, 'Whether the corporate take is in fact uncompensated at scale.').

omega_variable(
    maintainer_exit_binding_mechanism,
    'Is maintainer retention in the arrangement driven by structural barriers (scattered copyright, dependency entanglement) or by internalized obligation (identity, guilt, community expectation)?',
    'Post-exit trajectories of maintainers who handed off or archived projects: if the felt obligation dissolves after exit, the internalized component dominates; if practical entanglement persists, the structural component dominates.',
    'If internalized, effective suppression exceeds the structural measure and the maintainer seat sits nearer full identity-lock; if structural, relicensing reform (contributor agreements, foundation-held copyright) is the operative fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_exit_binding_mechanism, empirical, 'Structural versus internalized binding for maintainers.').

omega_variable(
    relicensing_leverage_realizability,
    'Can fragmented contributor copyright realistically be assembled to relicense mature projects toward reciprocity, or is that leverage permanently inert?',
    'Case studies of attempted mass-relicensing campaigns — contributor-clause rollouts, foundation copyright consolidation — measuring assembly cost and success rates.',
    'Realizable leverage lowers effective exit costs for the payer seats and weakens the ratchet sustaining the arrangement; inert leverage confirms the trap and supports the strong reading of the victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relicensing_leverage_realizability, empirical, 'Whether the exit path for payers is live or structurally blocked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t1998, permissive_license_text__corporate_moat_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement_basis(perm_tr_t1998, observed).
narrative_ontology:measurement(perm_tr_t2004, permissive_license_text__corporate_moat_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement_basis(perm_tr_t2004, observed).
narrative_ontology:measurement(perm_tr_t2010, permissive_license_text__corporate_moat_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement_basis(perm_tr_t2010, observed).
narrative_ontology:measurement(perm_tr_t2014, permissive_license_text__corporate_moat_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement_basis(perm_tr_t2014, observed).
narrative_ontology:measurement(perm_tr_t2019, permissive_license_text__corporate_moat_reading, theater_ratio, 2019, 0.34).
narrative_ontology:measurement_basis(perm_tr_t2019, observed).
narrative_ontology:measurement(perm_tr_t2021, permissive_license_text__corporate_moat_reading, theater_ratio, 2021, 0.36).
narrative_ontology:measurement_basis(perm_tr_t2021, observed).
narrative_ontology:measurement(perm_tr_t2026, permissive_license_text__corporate_moat_reading, theater_ratio, 2026, 0.4).
narrative_ontology:measurement_basis(perm_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(perm_be_t1998, permissive_license_text__corporate_moat_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement_basis(perm_be_t1998, observed).
narrative_ontology:measurement(perm_be_t2004, permissive_license_text__corporate_moat_reading, base_extractiveness, 2004, 0.38).
narrative_ontology:measurement_basis(perm_be_t2004, observed).
narrative_ontology:measurement(perm_be_t2010, permissive_license_text__corporate_moat_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement_basis(perm_be_t2010, observed).
narrative_ontology:measurement(perm_be_t2014, permissive_license_text__corporate_moat_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement_basis(perm_be_t2014, observed).
narrative_ontology:measurement(perm_be_t2019, permissive_license_text__corporate_moat_reading, base_extractiveness, 2019, 0.59).
narrative_ontology:measurement_basis(perm_be_t2019, observed).
narrative_ontology:measurement(perm_be_t2021, permissive_license_text__corporate_moat_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement_basis(perm_be_t2021, observed).
narrative_ontology:measurement(perm_be_t2026, permissive_license_text__corporate_moat_reading, base_extractiveness, 2026, 0.65).
narrative_ontology:measurement_basis(perm_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__corporate_moat_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'permissive licensing' decomposes into three epsilon-invariant readings of one kernel, linked here. This story carries the corporate-moat instantiation (epsilon ~0.65 over the standing permissive arrangement; victims = maintainers and contributing developers). The commons-coordination sibling measures near the coordination floor because its referent emphasizes friction elimination; the copyleft-counterfactual sibling measures high epsilon but relocates the actionable structure to the missing reciprocity clause. The commons reading is upstream (historically legitimated the instrument); this reading is downstream and cites its extraction outcomes. One label, three constraints — the epsilon difference across siblings is the disambiguation, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__corporate_moat_reading, institutional, 0.3).
constraint_indexing:directionality_override(permissive_license_text__corporate_moat_reading, organized, 0.55).
constraint_indexing:directionality_override(permissive_license_text__corporate_moat_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
