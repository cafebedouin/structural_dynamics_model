% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Proprietary Software Control as Legitimate Property Right
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the property_rights_reading of the contested
 *   software_control_legitimacy kernel: creators/vendors have legitimate
 *   authority to restrict use, modification, and distribution of software
 *   because that restriction is what enables recouping investment and
 *   sustaining commercial production. The arrangement genuinely coordinates
 *   something real — it lets funded development happen at scale — but it also
 *   draws a victim set: FOSS advocates and interoperability-seeking
 *   developers whose contribution and adaptation activity is foreclosed by
 *   the same legal machinery, and end users denied basic modification and
 *   repair rights. Extraction is moderate rather than severe because much
 *   proprietary software genuinely delivers a maintained, supported product
 *   in exchange for the restriction; the extraction is in the surplus beyond
 *   what investment-protection alone would require — lock-in,
 *   anti-interoperability provisions, and enforcement reach into adjacent
 *   development activity that does not compete with the original investment.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: agenda_setter/beneficiary (institutional/arbitrage) — sets license terms and enforces via EULA/DRM/litigation
 *   - venture_investors: beneficiary (powerful/arbitrage) — captures return contingent on enforceable restriction
 *   - foss_advocates: payer (organized/constrained) — denied any return-on-investment recognition for collaborative model
 *   - interoperability_seeking_developers: payer (moderate/constrained) — blocked from compatible-tool development by license and anti-circumvention law
 *   - end_users_denied_modification_rights: payer (powerless/trapped) — cannot inspect, repair, or adapt purchased software
 *   - technology_policy_courts: observer (institutional/analytical) — adjudicates the boundary between legitimate protection and overreach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.42).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.5).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Proprietary Software Control as Legitimate Property Right").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '0effa3b7-b678-4d03-bb4e-274efb66b304').
narrative_ontology:cs_kernel_codification('0effa3b7-b678-4d03-bb4e-274efb66b304', distributed).
narrative_ontology:cs_authority_grounding('0effa3b7-b678-4d03-bb4e-274efb66b304', extraction).
narrative_ontology:cs_interpretation_layer_present('0effa3b7-b678-4d03-bb4e-274efb66b304').
narrative_ontology:cs_reading_relation('0effa3b7-b678-4d03-bb4e-274efb66b304', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('0effa3b7-b678-4d03-bb4e-274efb66b304', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('0effa3b7-b678-4d03-bb4e-274efb66b304', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('0effa3b7-b678-4d03-bb4e-274efb66b304', foundational, creator_control_is_legitimate_property_entitlement).
narrative_ontology:cs_axiom_status(creator_control_is_legitimate_property_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('0effa3b7-b678-4d03-bb4e-274efb66b304', creator_control_is_legitimate_property_entitlement, conventional).
narrative_ontology:cs_axiom('0effa3b7-b678-4d03-bb4e-274efb66b304', secondary, investment_recoupment_justifies_restriction_scope).
narrative_ontology:cs_axiom_status(investment_recoupment_justifies_restriction_scope, holdable).
narrative_ontology:cs_axiom_grounding('0effa3b7-b678-4d03-bb4e-274efb66b304', investment_recoupment_justifies_restriction_scope, instrumental).
narrative_ontology:cs_reference_frame('0effa3b7-b678-4d03-bb4e-274efb66b304', copyright_backed_exclusive_control_norm).
narrative_ontology:cs_drift_state('0effa3b7-b678-4d03-bb4e-274efb66b304', post_foss_movement_maturity, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0effa3b7-b678-4d03-bb4e-274efb66b304', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, venture_investors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, enterprise_software_publishers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, interoperability_seeking_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users_denied_modification_rights).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, creator_investment_return_doctrine).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, commercial_sustainability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write license terms restricting copying, modification, and redistribution; enforce them through EULAs, DRM, litigation, and platform gatekeeping. Collect licensing revenue and investment returns directly from the restriction. Can lobby to strengthen copyright and DMCA-style enforcement further.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, beneficiary).

% Fund software development on the expectation that restricted distribution and use produce a monetizable, defensible asset. Their return depends on the enforceability of the property claim; they can redeploy capital elsewhere if the restriction regime weakens, unlike the developers and users bound to any given product.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, venture_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Bundle proprietary control into enterprise contracts, support agreements, and certification regimes, extracting recurring licensing revenue from lock-in. Benefit from the property framing because it legitimizes vendor lock-in as a normal cost of doing business rather than a captured market.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, enterprise_software_publishers, beneficiary,
    institutional, generational, mobile, global).

% Build alternative freely-licensed software but are structurally shut out of ecosystems, standards bodies, and procurement channels that treat proprietary control as the default legitimate arrangement. Denied any return on the collaborative investment their movement represents, since the property framing does not recognize non-ownership contribution as entitling anyone to control.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, constrained, global).

% Need to build compatible tools, plugins, or reverse-engineered interfaces but are blocked by license terms, anti-circumvention law, and patent threats. Their labor and business plans depend on access the property right explicitly withholds; litigation risk makes exit from compliance effectively closed.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, interoperability_seeking_developers, payer,
    moderate, biographical, constrained, national).

% Purchase or license software they cannot inspect, repair, or adapt to their needs; right-to-repair and modification are foreclosed by the same restriction that the vendor characterizes as protecting investment. Switching costs and format lock-in make leaving the ecosystem costly even when alternatives nominally exist.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users_denied_modification_rights, payer,
    powerless, biographical, trapped, global).

% Adjudicate copyright, DMCA, and licensing disputes, weighing the property-rights framing against interoperability, fair use, and right-to-repair claims. Their rulings shift how much enforcement weight the restriction can carry going forward.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, technology_policy_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Grants creators exclusive control over distribution and modification so that investment in software development can be recouped through licensing revenue, making continued production and maintenance economically viable.
% TRANSFER_FUNCTION: Moves the ability to inspect, modify, redistribute, and interoperate with software from users and downstream developers to the rights-holder, in exchange for the rights-holder bearing development risk and (in principle) sustaining the product.
% ABSENT_VOICES: FOSS communities and interoperability-seeking developers are structurally excluded from standards and procurement processes that assume proprietary control is the default legitimate arrangement; their objection — that collaborative, non-ownership models can sustain software without exclusive restriction — is treated as a market preference rather than a rival legitimacy claim.
% DISAPPEARANCE_RATIONALE: If the property-rights framing collapsed overnight, licensing revenue models, enterprise support contracts, and much of the venture-funded software industry's return logic would have to be rebuilt around alternative sustainability mechanisms (service revenue, patronage, public funding); vendors currently depending on restriction-enforced lock-in would lose their primary defensive moat.
% FOUNDING_PROBLEM: Software development requires substantial upfront investment (engineering time, testing, maintenance) with near-zero marginal cost of copying; without some mechanism to prevent unrestricted copying, creators feared they could not recoup investment or sustain ongoing development.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and investors attest the problem remains live — commercial software still requires funded, sustained engineering effort. Independent economists and the FOSS movement's own multi-decade track record (sustainable large-scale projects funded via services, foundations, and public goods models) attest that exclusive restriction is one sustainability mechanism among several, not the necessary one the property-rights reading treats it as; courts in interoperability and right-to-repair litigation have repeatedly found the restriction extends further than investment-protection alone would require.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).
:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) sits at a moderate level reflecting that the property claim does perform real coordination work (funding development, enabling maintenance commitments) alongside genuine rent extraction (lock-in beyond investment-recoupment, anti-interoperability enforcement). Suppression (0.5) is substantial because the restriction is backed by copyright law, DMCA-style anti-circumvention statutes, and litigation threat — real coercive machinery, not mere convention. Theater ratio is low (0.15) because the enforcement mostly does real suppressive work rather than performing it; there is little pure theater here. Accessibility collapse is moderate (0.4): alternatives (FOSS, alternative licensing) exist and are used, so collapse is far from total, unlike a genuine mountain. Resistance is substantial (0.55) — the FOSS movement, right-to-repair advocacy, and interoperability litigation constitute organized, sustained resistance to this reading's claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors, investors, and enterprise publishers sit near the beneficiary end of directionality: they collect licensing revenue and investment returns directly from the restriction and retain mobile-to-arbitrage exit (they can restructure business models or redeploy capital). FOSS advocates and interoperability developers sit near the target end: their labor and models are structurally denied recognition or market access by the same restriction; their exit is constrained because standards bodies, procurement, and platform ecosystems assume the property framing as default. End users sit furthest toward trapped — format and platform lock-in leave them with the least genuine exit despite bearing real costs from denied modification rights.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding development against zero-marginal-cost copying) remains partly live — sustained software development still requires resources — but the property-rights reading has not adjusted its restriction scope as the sustainability landscape has diversified (services revenue, foundation funding, public goods models proven viable by large FOSS projects). Classifying this as tangled_rope rather than snare or mountain prevents both errors: it is not pure extraction (real coordination — funded maintained software — occurs), and it is not natural law (identifiable beneficiaries exist and alternative models demonstrably work), so calling it a mountain would be a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    investment_protection_vs_restriction_overreach,
    'Where is the line between restriction that genuinely protects recoupment of development investment and restriction that extracts rent beyond what investment protection requires (e.g., anti-interoperability clauses, DRM reaching into unrelated use)?',
    'Comparative analysis of licensing terms against actual development cost structures; antitrust and right-to-repair litigation outcomes that test whether specific restrictions are necessary to investment recoupment or merely convenient to market power.',
    'If most restriction scope traces to genuine investment protection, the property-rights reading is closer to rope; if a large share is unrelated rent extraction, the tangled_rope classification understates the extractive component and a snare reading becomes more defensible for that subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_protection_vs_restriction_overreach, empirical, 'Whether restriction scope matches investment-protection need or exceeds it.').

omega_variable(
    property_framing_vs_alternative_kernel_readings,
    'Is software control best understood through the property-rights lens at all, or is the property framing itself a contestable choice among several legitimate legitimacy claims (freedom imperative, pragmatic openness, commons governance) that this reading treats as settled?',
    'Cross-reading comparison: examine whether jurisdictions, institutions, or communities that adopt a different kernel reading (e.g. copyleft licensing regimes reflecting commons_reading) sustain comparable levels of software development and maintenance without the property-rights restriction apparatus.',
    'If alternative readings sustain comparable outcomes, the property-rights reading''s claim to being the necessary or default legitimacy frame weakens substantially, supporting the view that its extraction is a contingent institutional choice rather than an inherent requirement of software economics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_framing_vs_alternative_kernel_readings, conceptual, 'Whether property-rights framing is a necessary economic requirement or one contested reading among several viable kernel interpretations.').

omega_variable(
    foss_advocate_return_recognition,
    'Does the FOSS movement''s collaborative production model constitute an ''investment'' whose non-recognition under the property-rights reading is itself an extraction, or is contribution to FOSS voluntary and non-transactional such that no return is owed?',
    'Examine whether FOSS contributors report harm/foreclosed opportunity from the property-rights regime''s refusal to recognize non-ownership contribution models in procurement and standards processes, versus contributors who describe their participation as genuinely non-transactional.',
    'If contributors demonstrate concrete foreclosed opportunity (denied procurement access, denied standards influence), the victim classification for foss_advocates strengthens; if participation is genuinely non-transactional by contributor account, the victim framing for that group should be narrowed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foss_advocate_return_recognition, preference, 'Whether non-recognition of FOSS contribution models constitutes a genuine cost to FOSS advocates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__property_rights_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__property_rights_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__property_rights_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(soft_tr_t32, software_control_legitimacy__property_rights_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__property_rights_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__property_rights_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__property_rights_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__property_rights_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__property_rights_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__property_rights_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__property_rights_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__property_rights_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__property_rights_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of four decompositions of the software_control_legitimacy kernel, split per the ε-invariance principle because the natural-language label 'software control legitimacy' covers structurally distinct claims with different beneficiary/victim sets and different ε values. property_rights_reading (this story, ε=0.42, tangled_rope) treats restriction as legitimate investment protection with FOSS advocates and interoperability developers as victims. freedom_imperative_reading treats proprietary restriction itself as the illegitimate extraction (inverted victim/beneficiary structure, likely higher ε from this reading's own lens). pragmatic_openness_reading treats the choice as methodology-neutral (likely lowest ε, closer to rope — both models coexist without either extracting). commons_reading treats software as negotiated shared infrastructure (distinct beneficiary structure again — collective governance bodies rather than either vendors or absolutist-freedom advocates). All four share the same kernel text/practice but are NOT the same constraint — each is measured by its own reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
