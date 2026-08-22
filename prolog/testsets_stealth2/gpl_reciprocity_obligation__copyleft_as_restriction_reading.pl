% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation — Copyleft-as-Restriction Reading (Proprietary Integration Bar)
 *   domain: legal/economic/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading — copyleft_as_restriction — of the
 *   contested kernel gpl_reciprocity_obligation. The standing arrangement
 *   under contest is the GPL's reciprocity machinery as actually enforced:
 *   distributing a derivative work obliges the distributor to license that
 *   whole work back under the GPL, with corresponding source. Read from the
 *   seat of firms that build proprietary products, that machinery operates as
 *   a standing prohibition on integrating mature free components into closed
 *   offerings, backed by copyright law, enforcement organizations, and a
 *   compliance-services industry. Epsilon is authored over that standing
 *   arrangement as this reading sees it; the reading's implied counterfactual
 *   — that lifting the restriction would shift gains to vendors, victimize
 *   commons contributors, and enable proprietary forks — is recorded in the
 *   omega variables (fork_enablement_counterfactual,
 *   kernel_reading_indexicality), not folded into the referent. Sibling
 *   readings (copyleft_as_freedom_reading, copyleft_as_commons_reading)
 *   instantiate different constraints from the same license text and are
 *   linked via the network section. Claim and metrics are independent: the
 *   claimed type records the structure I believe true (a real coordination
 *   function carrying a real asymmetric burden); the metrics record what the
 *   arrangement looks like from this reading's seat. KEY AGENTS (by
 *   structural relationship): - proprietary_software_vendors: Primary target
 *   (powerful/constrained) — bears the integration bar -
 *   enterprise_it_procurement: Secondary target (powerful/constrained) — pays
 *   avoidance and scanning costs - upstream_gpl_copyright_holders: Primary
 *   beneficiary (organized/mobile) — retains enforcement standing and
 *   exception leverage - free_software_downstream_developers: Beneficiary
 *   (organized/mobile) — receives returned improvements -
 *   end_users_of_gpl_software: Beneficiary (organized/mobile) — holds
 *   surviving source and modification rights - dual_license_vendors:
 *   Dual-positioned beneficiary/agenda-setter (powerful/arbitrage) —
 *   monetizes exceptions - fsf_licensing_enforcement: Agenda setter
 *   (institutional/identity_locked) — publishes, interprets, enforces -
 *   saas_cloud_operators: Excluded seat (institutional/arbitrage) — outside
 *   the trigger boundary - courts_adjudicating_license_disputes: Analytical
 *   observer (institutional/analytical) — fixes the license's legal character
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.71).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.62).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation — Copyleft-as-Restriction Reading (Proprietary Integration Bar)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "legal/economic/technological").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '3b6e5b4c-a508-4568-9524-71dd0d17f858').
narrative_ontology:cs_kernel_codification('3b6e5b4c-a508-4568-9524-71dd0d17f858', fixed_text).
narrative_ontology:cs_authority_grounding('3b6e5b4c-a508-4568-9524-71dd0d17f858', lineage).
narrative_ontology:cs_interpretation_layer_present('3b6e5b4c-a508-4568-9524-71dd0d17f858').
narrative_ontology:cs_reading_relation('3b6e5b4c-a508-4568-9524-71dd0d17f858', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b6e5b4c-a508-4568-9524-71dd0d17f858', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('3b6e5b4c-a508-4568-9524-71dd0d17f858', foundational, proprietary_integration_is_legitimate_business_activity).
narrative_ontology:cs_axiom_status(proprietary_integration_is_legitimate_business_activity, holdable).
narrative_ontology:cs_axiom_grounding('3b6e5b4c-a508-4568-9524-71dd0d17f858', proprietary_integration_is_legitimate_business_activity, deontological).
narrative_ontology:cs_axiom('3b6e5b4c-a508-4568-9524-71dd0d17f858', secondary, reciprocity_scope_exceeds_coordination_cost).
narrative_ontology:cs_axiom_status(reciprocity_scope_exceeds_coordination_cost, holdable).
narrative_ontology:cs_axiom_grounding('3b6e5b4c-a508-4568-9524-71dd0d17f858', reciprocity_scope_exceeds_coordination_cost, instrumental).
narrative_ontology:cs_reference_frame('3b6e5b4c-a508-4568-9524-71dd0d17f858', distribution_triggered_integration_prohibition).
narrative_ontology:cs_drift_state('3b6e5b4c-a508-4568-9524-71dd0d17f858', contemporary_saas_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b6e5b4c-a508-4568-9524-71dd0d17f858', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, upstream_gpl_copyright_holders).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, free_software_downstream_developers).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, end_users_of_gpl_software).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_license_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enterprise_it_procurement).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, license_condition_enforceability).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, reciprocity_as_enclosure_defense).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes and maintains the GNU General Public License texts, interprets them through its licensing team and published FAQ, and pursues compliance when distribution occurs without corresponding source. Its staff, fundraising, and reputation are organized around the license's continued integrity; stepping back from that role would leave its core purpose unstaffed.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, fsf_licensing_enforcement, agenda_setter,
    institutional, generational, identity_locked, global).

% Individual and corporate authors who release code under the GPL. Holding copyright, they retain standing to enforce the terms, to grant or refuse exception agreements, and to relicense where they have gathered assignments. Improvements arriving from other GPL projects raise the value of what they maintain.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, upstream_gpl_copyright_holders, beneficiary,
    organized, biographical, mobile, global).

% Build on GPL libraries and applications knowing that anything they distribute inherits the same terms. They receive a body of working code they could not have assembled alone and pass the same availability forward to their own recipients.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, free_software_downstream_developers, beneficiary,
    organized, biographical, mobile, global).

% Run systems built on GPL components — servers, phones, routers — and hold formal rights to source, modification, and redistribution that survive whatever vendor shipped the binary. Few exercise these rights directly; their existence disciplines shippers considering a closed release.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, end_users_of_gpl_software, beneficiary,
    organized, biographical, mobile, global).

% Hold copyright to widely used GPL codebases and sell commercial exception licenses alongside the free terms. Every prospective integrator who cannot accept copyleft becomes a sales conversation. They set the exception price and decide which license version their code carries.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_license_vendors, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_license_vendors, agenda_setter).

% Ship products that would benefit from GPL components — kernels, compilers, media stacks, cryptographic libraries. Distributing those components obliges them to release their own source on the same terms, so they either open significant product code, purchase exceptions where available, engineer around the components, or forgo them. Substitutes exist but demand rework and surrender maturity.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Corporate engineering and purchasing groups that maintain no-GPL-in-shipped-product policies. They pay for license scanning, legal review, and sometimes inferior commercial alternatives purely to keep copyleft obligations out of their supply chain, and they absorb the schedule delays those precautions cause.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enterprise_it_procurement, payer,
    powerful, biographical, constrained, global).

% Run GPL-licensed software on their own servers and sell access over networks. Because they distribute nothing, the license's trigger never fires: they improve the code internally, keep those improvements private, and compete with the upstream projects without joining their terms. They have strong reasons to resist any widening of the trigger to network use.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, saas_cloud_operators, excluded,
    institutional, biographical, arbitrage, global).

% Hear infringement and contract claims involving the GPL — German regional courts enforcing source-delivery obligations, the US Federal Circuit treating violation as copyright infringement in Jacobsen v. Katzer. Their characterizations of the license as condition or contract shape what every other party can rely on.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, courts_adjudicating_license_disputes, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_license_vendors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in pooled code production: by making redistribution conditional on passing the same rights and source forward, it keeps improvements inside the shared pool instead of letting integrators privatize them, converting scattered contributions into a durable, growing body of usable code.
% TRANSFER_FUNCTION: Moves source-code rights and improvement obligations from distributors back to recipients — anyone shipping a derivative must hand over full source under the same terms — and, commercially, moves bargaining leverage from would-be integrators to copyright holders, who convert that leverage into exception-fee revenue.
% ABSENT_VOICES: SaaS operators sit outside the trigger and are absent from compliance conversations they materially affect; embedded-device end users routinely receive binaries without the promised source offers and almost never appear in enforcement dockets; hobbyist contributors are rarely consulted when enforcement strategy or relicensing decisions are made by foundations and corporate stewards.
% DISAPPEARANCE_RATIONALE: Overnight repeal of the reciprocity term would produce proprietary forks of the kernel, compilers, and core libraries within quarters; contribution incentives would invert as employers captured employee-published work without return; the mixed free/commercial software economy would reorganize around permissive terms, paid exceptions, or fully closed stacks; and the compliance industry built on the obligation would evaporate.
% FOUNDING_PROBLEM: In the early 1980s, freely shared code was repeatedly taken proprietary — the canonical episode being a laboratory colleague's printer-control program withheld as proprietary after its author left — leaving prior collaborators unable to fix or extend what they had helped build. The GPL was drafted to make the freedom to study, modify, and redistribute irrevocable by binding it to every redistribution.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: US and German case law (Jacobsen v. Katzer; Munich regional-court rulings) treats the obligation as enforceable, presupposing a protected interest; corporate open-source offices at firms openly hostile to copyleft nonetheless maintain compliance programs, conceding the terms' operative reality; academic histories of the free-software movement independently document the pre-GPL enclosure episodes. No source outside the beneficiary set attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71) because the obligation reaches the entire derivative work, not merely the borrowed component: a firm wanting one mature library must open product-scale source, purchase an exception where one exists, engineer around the component, or forgo it — each path taxing the business model this reading centers. Suppression (0.62) reflects enforcement that is real but bounded: copyright actions, compliance programs, and scanner-driven procurement fear deter integration, yet permissive substitutes, clean-room rewrites, and SaaS routing leave genuine alternatives — hence accessibility_collapse sits mid-range (0.45) and resistance is substantial (0.55): no-GPL procurement policies, permissive-ecosystem funding, and periodic litigation challenges are organized pushback, and vendors have repeatedly demonstrated coalition capacity (foundation-funded permissive stacks) rather than isolated compliance. Theater (0.34) tracks the growth of compliance ritual — scanning reports, audit files, policy binders — whose liability-cover share has grown relative to functional share. All three series share one eight-point grid (t0 approximates 1989/GPLv1 through t35 approximates 2024): extractiveness climbs with kernel, embedded, and Android ubiquity through t20, then plateaus as the SaaS loophole and permissive substitution offset further spread; suppression_requirement rises as enforcement professionalizes, then steadies; theater rises monotonically. No cyclical oscillation is asserted. Suppression here is predominantly structural (license terms plus case law) with an internalized stratum — procurement fear culture — quantified as open in the precautionary_overcompliance_share omega.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From proprietary_software_vendors and enterprise_it_procurement the arrangement lands as a costly bar with constrained exits; from upstream_gpl_copyright_holders, free_software_downstream_developers, and end_users_of_gpl_software it lands as protection that returns contributions and guarantees freedoms. dual_license_vendors and proprietary_software_vendors hold comparable market power yet sit at opposite ends of the relationship — differentiated not by size but by copyright posture, which converts the same license text into revenue for one and rework for the other. The fsf_licensing_enforcement seat is identity-fused: its organizational self-concept is the license's integrity, so its exit is identity_locked rather than strategic, and a break in that identity frame (a successor foundation treating enforcement as optional) would soften the arrangement's suppressive force faster than any doctrinal change. Courts see only characterization questions. The engine computes these per-seat classifications from the structural data; nothing in the claimed type adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries — upstream_gpl_copyright_holders, free_software_downstream_developers, end_users_of_gpl_software, dual_license_vendors — derive low directionality (subsidized or near-symmetric; dual-license holders nearest the beneficiary pole given arbitrage-grade monetization of the terms themselves). Declared victims — proprietary_software_vendors and enterprise_it_procurement — derive high directionality, amplified by constrained exit and global spatial scope: verifying source obligations across jurisdictions is hard, so the engine scales effective extraction upward for these seats. saas_cloud_operators are excluded rather than coordinated: the trigger boundary places them outside the arrangement's reach, which is precisely the exclusion the AGPL was later written to close. No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms already separate every seat the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — freely shared code being taken proprietary and prior collaborators locked out — is corroborated as historically real and still live (cloud capture of community projects, recurring fork attempts), so mandatrophy is not resolved and no sunset is declared. Classifying as tangled_rope rather than snare keeps the genuine coordination achievement visible inside the reading's complaint: the same terms that bar closed integration are what make pooled development stable, and the transfer they compel (source back to recipients) is the mechanism, not a disguise. Conversely, refusing rope keeps the asymmetric burden on the record rather than laundering it as pure coordination cost. If the founding problem ever died — enclosure rendered impossible by other means — the arrangement would drift toward piton (ritual compliance without function), which the rising theater_ratio series is positioned to detect; the mismatch consumer should watch founding_problem_status against disappearance_verdict accordingly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (copyleft_as_restriction) of the kernel gpl_reciprocity_obligation; how would instantiating the sibling readings — copyleft_as_freedom or copyleft_as_commons — change the beneficiary/victim structure and epsilon of the same underlying license terms?',
    'Compare the three family stories'' declared beneficiaries, victims, and epsilon over the fixed referent (the enforced reciprocity arrangement). Divergence localizes the dispute to seat selection rather than to facts about the license text.',
    'Under the commons or freedom readings the same terms compute as low-extraction protective coordination with commons-side beneficiaries; under this reading they compute as a high-extraction bar on commercial integration with vendor-side victims. Which reading prevails in doctrine and procurement determines which structure the engine should weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-selection dependence of the constraint''s structure (committer-frame omega).').

omega_variable(
    fork_enablement_counterfactual,
    'Does weakening or unenforcing reciprocity actually deliver the structural delta this reading implies — gains accruing to proprietary vendors, losses falling on commons contributors, and proliferation of proprietary forks?',
    'Track fork formation and contribution flows after adverse-enforcement rulings or mass permissive migrations (post-Jacobsen compliance waves; projects that relicensed to Apache) and compare against matched projects that retained copyleft.',
    'Confirmation supports the reading''s causal story and raises the stakes of its adoption in policy; refutation indicates the restriction framing overstates vendor gains and commons losses, collapsing the delta toward zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fork_enablement_counterfactual, empirical, 'Whether the reading''s implied counterfactual (vendor benefit, commons victimization, proprietary forks) holds.').

omega_variable(
    contract_or_condition_characterization,
    'Is the GPL a contract requiring assent or a bare copyright condition attaching automatically to distribution?',
    'Appellate harmonization across jurisdictions: German regional courts have treated the license as contract-adjacent; the US Federal Circuit in Jacobsen v. Katzer treated violation as copyright infringement of a condition.',
    'Contract characterization raises enforcement friction (assent and privity requirements) and lowers effective suppression; condition characterization strengthens automatic reach and raises the burden this reading measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contract_or_condition_characterization, empirical, 'Legal characterization of the license determining enforcement mechanics.').

omega_variable(
    network_use_trigger_boundary,
    'Does offering GPL code''s function over a network count as the distribution that triggers reciprocity?',
    'AGPL adoption rates among infrastructure projects, plus any judicial or legislative extension of the trigger to service delivery.',
    'A large fraction of potential vendor-side burden is currently routed around via SaaS deployment; extending the trigger would raise measured extraction sharply, while leaving the boundary intact confines this reading''s scope to shipped products.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_use_trigger_boundary, empirical, 'Scope boundary of the reciprocity trigger (distribution vs. network service).').

omega_variable(
    precautionary_overcompliance_share,
    'How much of the burden this reading measures is imposed by the license''s actual operation versus precautionary avoidance — procurement bans and scanner-driven rejection of GPL code that the terms would never have reached?',
    'Audit corporate license policies against actual linking and distribution footprints; compare firms with identical technical exposure but different policy cultures.',
    'If most of the burden is self-imposed caution, suppression is partly internalized (policy culture persisting past any legal requirement) rather than externally enforced, and the reading''s complaint is aimed partly at its own adherents'' practices; the effective-suppression estimate falls accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precautionary_overcompliance_share, empirical, 'Structural versus internalized share of the measured suppression (suppression-mechanism ambiguity omega).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_restriction_reading_tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gpl_restriction_reading_tr_t0, observed).
narrative_ontology:measurement(gpl_restriction_reading_tr_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(gpl_restriction_reading_tr_t5, observed).
narrative_ontology:measurement(gpl_restriction_reading_tr_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(gpl_restriction_reading_tr_t10, observed).
narrative_ontology:measurement(gpl_restriction_reading_tr_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(gpl_restriction_reading_tr_t15, observed).
narrative_ontology:measurement(gpl_restriction_reading_tr_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(gpl_restriction_reading_tr_t20, observed).
narrative_ontology:measurement(gpl_restriction_reading_tr_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(gpl_restriction_reading_tr_t25, observed).
narrative_ontology:measurement(gpl_restriction_reading_tr_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(gpl_restriction_reading_tr_t30, observed).
narrative_ontology:measurement(gpl_restriction_reading_tr_t35, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 35, 0.34).
narrative_ontology:measurement_basis(gpl_restriction_reading_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gpl_restriction_reading_be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(gpl_restriction_reading_be_t0, observed).
narrative_ontology:measurement(gpl_restriction_reading_be_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(gpl_restriction_reading_be_t5, observed).
narrative_ontology:measurement(gpl_restriction_reading_be_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(gpl_restriction_reading_be_t10, observed).
narrative_ontology:measurement(gpl_restriction_reading_be_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(gpl_restriction_reading_be_t15, observed).
narrative_ontology:measurement(gpl_restriction_reading_be_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(gpl_restriction_reading_be_t20, observed).
narrative_ontology:measurement(gpl_restriction_reading_be_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 25, 0.69).
narrative_ontology:measurement_basis(gpl_restriction_reading_be_t25, observed).
narrative_ontology:measurement(gpl_restriction_reading_be_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(gpl_restriction_reading_be_t30, observed).
narrative_ontology:measurement(gpl_restriction_reading_be_t35, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 35, 0.71).
narrative_ontology:measurement_basis(gpl_restriction_reading_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl_restriction_reading_su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(gpl_restriction_reading_su_t0, observed).
narrative_ontology:measurement(gpl_restriction_reading_su_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement_basis(gpl_restriction_reading_su_t5, observed).
narrative_ontology:measurement(gpl_restriction_reading_su_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(gpl_restriction_reading_su_t10, observed).
narrative_ontology:measurement(gpl_restriction_reading_su_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(gpl_restriction_reading_su_t15, observed).
narrative_ontology:measurement(gpl_restriction_reading_su_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(gpl_restriction_reading_su_t20, observed).
narrative_ontology:measurement(gpl_restriction_reading_su_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(gpl_restriction_reading_su_t25, observed).
narrative_ontology:measurement(gpl_restriction_reading_su_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement_basis(gpl_restriction_reading_su_t30, observed).
narrative_ontology:measurement(gpl_restriction_reading_su_t35, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(gpl_restriction_reading_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'GPL reciprocity obligation' covers three structurally distinct claims indexed by seat. This story (copyleft_as_restriction_reading) authors high epsilon over the standing arrangement as seen from proprietary-integration seats; copyleft_as_freedom_reading authors the user-freedom structure; copyleft_as_commons_reading authors the anti-enclosure structure with low epsilon. The commons reading is doctrinally upstream — its account anchors enforcement legitimacy in the enclosure-prevention record — and influences both siblings; this reading competes with both in procurement, standards, and policy arenas without logically eliminating either. Edges here point to both siblings; each sibling reciprocates in its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
