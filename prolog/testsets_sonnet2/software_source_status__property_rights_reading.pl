% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Proprietary Software Licensing as Legitimate Property Right
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the property-rights reading of the contested
 *   kernel over software's source status: source code is a proprietary asset,
 *   and licensing restrictions on access, inspection, and modification are
 *   legitimate exercises of ownership analogous to physical property rights.
 *   Users hold contractual rights defined by the license, not inherent rights
 *   to inspect or alter the code they run. This is a distinct constraint from
 *   the freedom-imperative, pragmatic-development, and utilitarian-hybrid
 *   readings of the same kernel — those are separate stories with their own
 *   ε, beneficiaries, and classification, linked here only by network
 *   reference, per the ε-invariance principle. Enforcement mechanisms
 *   (copyright, the DMCA's anti-circumvention provisions, EULAs, patent
 *   claims) have hardened substantially since 1980 as software shifted from a
 *   niche good to the substrate of the economy, which the temporal
 *   measurements trace.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: agenda_setter/beneficiary (institutional/arbitrage) — write and enforce licenses, capture the revenue
 *   - venture_capital_investors: beneficiary (powerful/arbitrage) — fund firms on the defensibility thesis
 *   - end_users_barred_from_modification: payer (powerless/constrained) — cannot inspect, fix, or adapt what they run
 *   - independent_repair_technicians: payer (moderate/trapped) — excluded from diagnostic access needed to compete
 *   - free_software_advocates: excluded (organized/mobile) — reject the property framing outright but are not licensing parties
 *   - courts_and_ip_regulators: observer (institutional/analytical) — set the actual enforceable boundary of the claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.58).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.62).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Proprietary Software Licensing as Legitimate Property Right").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '8afd9fc2-a507-45ce-9af5-8bc5689a3bc7').
narrative_ontology:cs_kernel_codification('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', formalized).
narrative_ontology:cs_authority_grounding('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', extraction).
narrative_ontology:cs_interpretation_layer_present('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7').
narrative_ontology:cs_reading_relation('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', foundational, labor_desert_grounds_exclusive_control).
narrative_ontology:cs_axiom_status(labor_desert_grounds_exclusive_control, holdable).
narrative_ontology:cs_axiom_grounding('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', labor_desert_grounds_exclusive_control, deontological).
narrative_ontology:cs_axiom('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', foundational, restriction_is_legitimate_absent_coercive_defect).
narrative_ontology:cs_axiom_status(restriction_is_legitimate_absent_coercive_defect, holdable).
narrative_ontology:cs_axiom_grounding('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', restriction_is_legitimate_absent_coercive_defect, conventional).
narrative_ontology:cs_reference_frame('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', anglo_american_copyright_property_framework).
narrative_ontology:cs_drift_state('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', post_dmca_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8afd9fc2-a507-45ce-9af5-8bc5689a3bc7', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, enterprise_software_shareholders).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, end_users_barred_from_modification).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_repair_technicians).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, downstream_interoperability_developers).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, labor_desert_theory_of_ip).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, innovation_incentive_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write license terms, enforce them through DRM, anti-circumvention litigation, and copyright/patent claims, and set the terms under which source code may be viewed, modified, or redistributed. Capture licensing and subscription revenue directly and can relocate corporate structure to favorable IP jurisdictions.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, proprietary_software_vendors, beneficiary).

% Fund proprietary software firms on the expectation that enforceable exclusivity over source code creates a defensible moat and predictable returns; can exit any single investment and reallocate capital toward whichever licensing regime yields the highest return.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, venture_capital_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Hold equity whose value depends substantially on the enforceability of restrictions against copying, reverse-engineering, and modification of the firm's codebase; can diversify or divest but benefit collectively from the legal regime's stability.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, enterprise_software_shareholders, beneficiary,
    organized, generational, mobile, global).

% Purchase or license software but cannot inspect, audit, fix, or adapt it; when the vendor stops supporting a product or a bug goes unfixed, they have no lawful recourse but to buy a new license or replacement product. Switching costs and format lock-in make exit costly rather than impossible.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, end_users_barred_from_modification, payer,
    powerless, biographical, constrained, global).

% Are prevented by license terms and anti-circumvention law from accessing diagnostic software or firmware needed to repair devices they do not manufacture, forcing customers back to the vendor's authorized service channel and excluding technicians from a market they could otherwise serve.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_repair_technicians, payer,
    moderate, biographical, trapped, national).

% Want to build compatible tools, plugins, or import/export bridges but are blocked by closed formats and prohibitions on reverse engineering in the license; must either reinvent functionality independently, license access at the vendor's price, or abandon the interoperability goal.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, downstream_interoperability_developers, payer,
    moderate, biographical, constrained, global).

% Argue that restricting a user's ability to run, study, modify, and share software is an ethical injustice regardless of contractual consent, but their framework is not the one courts or license drafters operate under; they build and promote alternatives rather than participating in the property-rights license negotiation itself.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_software_advocates, excluded,
    organized, civilizational, mobile, global).

% Adjudicate copyright, patent, and contract disputes arising from software licenses, interpret the scope of fair use and reverse-engineering exceptions, and can narrow or expand the enforceability of restrictive terms through rulings and legislation.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, courts_and_ip_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enforceable exclusivity over source code lets a firm internalize the returns on its development investment, coordinating capital, engineering labor, and long-term maintenance commitments around a product that would otherwise be instantly and costlessly copyable.
% TRANSFER_FUNCTION: Moves control over modification, inspection, and redistribution from the purchaser/licensee to the vendor, and moves the economic surplus from unrestricted use (repair, adaptation, resale of derivative value) to the vendor's licensing and subscription revenue.
% ABSENT_VOICES: Free software advocates and independent repair technicians would object that restricting inspection and modification degrades user autonomy and creates artificial scarcity around a non-rival good, but they are not parties to the license they must accept to use the product — the contract is offered on a take-it-or-leave-it basis.
% DISAPPEARANCE_RATIONALE: If proprietary restrictions on source code disappeared overnight, the current revenue model for licensed software would collapse into a service/support model, venture funding predicated on defensible moats would need new theses, and independent repair and interoperability markets would open substantially — a real reorganization of the software economy, not a cosmetic one.
% FOUNDING_PROBLEM: Software is trivially and perfectly copyable; without some enforceable exclusivity, firms and individual developers could not capture returns on the substantial fixed cost of writing and maintaining complex code, threatening the incentive to invest in its creation at all.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and their investors attest the incentive problem remains fully live. Independent economists studying open-source and dual-license firms, and courts narrowing copyright/DMCA scope in cases like right-to-repair litigation, attest from outside the beneficiary set that substantial software value is created and sustained without full exclusivity, undermining the strong form of the founding claim even as a weaker version persists.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that vendors capture returns substantially exceeding marginal distribution cost once software has been written — a widening gap traced by the temporal series as the industry matured from 1980 to 2025. Suppression (0.62) is higher than extraction because the property-rights reading depends on active legal enforcement (DMCA anti-circumvention, EULA litigation, patent assertion) to prevent alternatives (inspection, modification, reverse-engineering, competing repair markets) rather than resting on unforced preference. Theater is modest (0.22): licensing enforcement performs real gatekeeping, not mostly ritual, though a growing share (rising to 0.22) is defensive litigation theater rather than functional IP protection. Accessibility collapse (0.6) is moderate-high because once a EULA is accepted alternatives are substantially foreclosed for that instance, but free/open alternatives exist in parallel for those willing to switch products — this is why collapse is not mountain-grade (~0.85+). Resistance (0.55) reflects organized pushback (right-to-repair movements, free software advocacy, antitrust scrutiny) that is real but has not overturned the basic enforceability of the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/investor seat, the arrangement reads as legitimate property protection enabling investment — a rope. From the end-user or repair-technician seat, the same license terms read as coercive exclusion from goods they have effectively purchased — closer to a snare. The engine computes these divergent per-seat classifications from the declared power/exit/beneficiary structure; the tangled_rope claim asserts that BOTH the coordination function and the asymmetric extraction are real and co-occurring, not that one seat's perception is mistaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors, their investors, and shareholders sit near the full-beneficiary end: they set the terms, capture the surplus, and can relocate capital or corporate structure (arbitrage-grade exit) if any single jurisdiction's IP regime becomes unfavorable. End users, repair technicians, and interoperability developers sit near the full-target end: they bear the restriction, have constrained-to-trapped exit (switching costs, format lock-in, exclusive diagnostic access), and cannot negotiate the license terms individually. This is a textbook tangled_rope directionality split — the same licensing structure that lets vendors coordinate investment around a defensible product is the structure that extracts from users who have no bargaining power over its terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — trivial copyability threatening the incentive to invest in software creation — is contested rather than resolved: the strong-exclusivity claim is undermined by the empirical success of open-source and dual-license firms, but the weaker claim (some exclusivity is needed for some business models) remains defensible. Classifying this as tangled_rope rather than snare prevents mislabeling the incentive-coordination function as pure extraction: real investment coordination does occur, and it is not solely captured rent. Classifying it as tangled_rope rather than rope prevents treating the arrangement as costless coordination: identifiable victims (repair technicians, interoperability developers, users) bear structural costs the coordination story does not fully account for, and active enforcement (not voluntary preference) sustains the exclusivity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incentive_thesis_empirical_status,
    'Does enforceable source-code exclusivity actually cause a higher rate or quality of software investment than would occur under a weaker-property or open regime, or does the counterfactual investment level differ less than the property-rights reading assumes?',
    'Comparative study of investment, maintenance quality, and innovation rates across matched proprietary and open-source/dual-license firms and ecosystems over multi-decade horizons, controlling for market segment.',
    'If exclusivity is not shown to causally increase investment beyond what alternative funding models achieve, the coordination-function claim underlying the tangled_rope classification weakens substantially and the constraint reads closer to a snare (extraction with a decorative coordination story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_thesis_empirical_status, empirical, 'Whether the labor/investment incentive thesis underlying property-rights licensing is empirically well-supported.').

omega_variable(
    kernel_framing_choice,
    'Is the underlying kernel (software''s source status) better framed as a property-rights question (who owns the code) or as an access/capability question (who can run, study, modify, and share it)? The property-rights reading and the freedom-imperative reading do not merely disagree on values — they frame the relevant unit of analysis differently (asset vs. capability), which changes what counts as a ''cost'' at all.',
    'No empirical resolution exists; this is a framing choice that different legal and ethical traditions have settled differently (Anglo-American copyright tradition vs. the free software movement''s capability framing).',
    'Under the property-rights framing (this story), restriction is a legitimate exercise of a right and ''victims'' are more precisely ''non-owners bearing the cost of others'' legitimate rights.'' Under the capability framing (the freedom_imperative sibling), the same restriction is itself the harm. The classification computed here (tangled_rope) is stable under the property-rights framing chosen for this story; adopting the capability framing instead would likely author a much higher extractiveness and shift the classification toward snare in that sibling story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether property-rights framing or capability/access framing is the correct unit of analysis for the underlying kernel — this story deliberately adopts the property-rights framing and authors ε accordingly.').

omega_variable(
    vindicated_proposition_vs_beneficiary_boundary,
    'Is ''innovation incentive thesis'' genuinely a vindicated proposition (a doctrine the arrangement''s operation supports) or does its persistence actually function to benefit specific rent-collecting agents who invoke it strategically, making it closer to a beneficiary-serving narrative than a disinterested doctrine?',
    'Trace citation and lobbying patterns: does the innovation-incentive argument appear predominantly in contexts where it serves the arguing party''s direct financial interest, versus in independent academic or judicial reasoning?',
    'If the doctrine functions predominantly as strategic cover, it should potentially be re-authored as a mechanism of beneficiary capture rather than a neutral vindicated proposition, which would sharpen rather than soften the extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vindicated_proposition_vs_beneficiary_boundary, conceptual, 'Whether the innovation-incentive doctrine is a genuine vindicated proposition or a strategic narrative serving concentrated beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_source_status__property_rights_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(soft_tr_t1990, software_source_status__property_rights_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__property_rights_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__property_rights_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(soft_tr_t2018, software_source_status__property_rights_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement(soft_tr_t2025, software_source_status__property_rights_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_source_status__property_rights_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(soft_be_t1990, software_source_status__property_rights_reading, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement(soft_be_t2000, software_source_status__property_rights_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(soft_be_t2010, software_source_status__property_rights_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(soft_be_t2018, software_source_status__property_rights_reading, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(soft_be_t2025, software_source_status__property_rights_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_source_status__property_rights_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(soft_su_t1990, software_source_status__property_rights_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(soft_su_t2000, software_source_status__property_rights_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(soft_su_t2010, software_source_status__property_rights_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(soft_su_t2018, software_source_status__property_rights_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(soft_su_t2025, software_source_status__property_rights_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.12).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked constraints decomposing the natural-language 'software is/isn't intellectual property' debate per the ε-invariance principle. Each reading of the software_source_status kernel is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification: property_rights_reading (this story, tangled_rope, ε=0.58), freedom_imperative_reading (ethical-injustice framing, expected higher ε and different victim set), pragmatic_development_reading (methodology framing, expected lower ε/more rope-like), and utilitarian_hybrid_reading (context-dependent, expected mixed/moderate ε). The four are linked bidirectionally via affects_constraints rather than merged into one story with an observable parameter, because measuring 'the same' software licensing arrangement through each reading's lens yields materially different ε values — the signature of multiple constraints sharing one colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
