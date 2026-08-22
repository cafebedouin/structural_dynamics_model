% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation — Copyleft as Freedom Reading
 *   domain: legal/technological
 *
 * SUMMARY:
 *   This story instantiates the copyleft_as_freedom_reading of the
 *   gpl_reciprocity_obligation kernel: the GPL's requirement that distributed
 *   derivatives carry the same license and source exists, on this reading, to
 *   preserve end-user freedoms by making proprietary capture of collectively
 *   built code impossible. The epsilon referent is the standing arrangement
 *   under contest — the reciprocity obligation itself — assessed by this
 *   reading's own lights: the burden it imposes on proprietary integrators is
 *   real but is deemed the enforcement mechanism of the freedom guarantee
 *   rather than a taking from a legitimate activity, hence the moderate
 *   epsilon (0.38). The claim and metrics are independent authored facts: the
 *   reading claims tangled_rope because it honestly concedes the structure
 *   both coordinates users into a commons AND extracts from integrators
 *   through the same license terms, while insisting the coordination function
 *   is primary and the extraction component legitimate. CONSTRAINT FAMILY:
 *   this is one of three sibling readings of the same kernel text. The
 *   copyleft_as_restriction_reading would author substantially higher epsilon
 *   for the identical arrangement (the integrator burden as pure
 *   business-model restriction); the copyleft_as_commons_reading would author
 *   moderate epsilon with the beneficiary emphasis shifted from individual
 *   users to the shared code pool. Each sibling is a separate constraint
 *   story with its own epsilon, linked via network.affects_constraints. Time
 *   mapping: T=0 is 1989 (GPLv1), T=2 GPLv2 and Linux adoption, T=18 GPLv3,
 *   T=36 approximately 2025.
 *
 * KEY AGENTS:
 *   - downstream_users: primary beneficiary (moderate/mobile) — receives guaranteed four-freedom access to code and all derivatives at no charge
 *   - free_software_developers: dual-positioned contributor (organized/mobile) — receives guaranteed improvement inflow, owes the same reciprocity on their own distributions
 *   - proprietary_integrators: primary target (powerful/constrained) — bears source-disclosure and license-reciprocity demands on derivative distribution
 *   - embedded_device_vendors: enforcement-facing target (organized/constrained) — ship GPL firmware in products; the most frequent defendants
 *   - free_software_foundation: agenda-setter (institutional/identity_locked) — holds copyrights, maintains the license text, defines authoritative interpretation
 *   - software_freedom_conservancy: enforcement administrator (institutional/constrained) — negotiates and litigates compliance; collects settlement-linked funding
 *   - dual_licensing_vendors: commercial beneficiary (powerful/mobile) — monetize the proprietary-distribution bar through paid exceptions
 *   - server_side_deployers: arbitrage beneficiary (institutional/arbitrage) — take the commons server-side where the distribution trigger never attaches
 *   - permissive_license_advocates: excluded voice (organized/mobile) — hold that freedom includes the freedom to make proprietary derivatives
 *   - copyright_courts: analytical observer (institutional/analytical) — adjudicate enforceability and standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.38).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.75).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation — Copyleft as Freedom Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "legal/technological").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'da1e4da0-4359-464e-b8d8-f97caa984fab').
narrative_ontology:cs_kernel_codification('da1e4da0-4359-464e-b8d8-f97caa984fab', fixed_text).
narrative_ontology:cs_authority_grounding('da1e4da0-4359-464e-b8d8-f97caa984fab', lineage).
narrative_ontology:cs_interpretation_layer_present('da1e4da0-4359-464e-b8d8-f97caa984fab').
narrative_ontology:cs_reading_relation('da1e4da0-4359-464e-b8d8-f97caa984fab', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_reading_relation('da1e4da0-4359-464e-b8d8-f97caa984fab', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('da1e4da0-4359-464e-b8d8-f97caa984fab', foundational, end_user_four_freedoms_paramount).
narrative_ontology:cs_axiom_status(end_user_four_freedoms_paramount, holdable).
narrative_ontology:cs_axiom_grounding('da1e4da0-4359-464e-b8d8-f97caa984fab', end_user_four_freedoms_paramount, deontological).
narrative_ontology:cs_axiom('da1e4da0-4359-464e-b8d8-f97caa984fab', foundational, reciprocity_is_freedom_enforcement_not_restriction).
narrative_ontology:cs_axiom_status(reciprocity_is_freedom_enforcement_not_restriction, holdable).
narrative_ontology:cs_axiom_grounding('da1e4da0-4359-464e-b8d8-f97caa984fab', reciprocity_is_freedom_enforcement_not_restriction, instrumental).
narrative_ontology:cs_reference_frame('da1e4da0-4359-464e-b8d8-f97caa984fab', four_freedoms_guarantee_framework).
narrative_ontology:cs_drift_state('da1e4da0-4359-464e-b8d8-f97caa984fab', contemporary_saas_ai_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('da1e4da0-4359-464e-b8d8-f97caa984fab', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_developers).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, dual_licensing_vendors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, server_side_deployers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, embedded_device_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_conservancy).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_developers).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_doctrine).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_reciprocity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Anyone who runs, studies, modifies, or redistributes GPL-licensed programs. Every copy they receive, including derivatives, arrives with source access and redistribution rights attached. They bear no obligation unless they themselves distribute modified versions. Alternative software exists and switching is possible, but the guarantee follows the code they already rely on at no charge to them.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    moderate, generational, mobile, global).

% Developers who publish work under the GPL and build on others' GPL work. They receive a guaranteed inflow of every downstream improvement to their code, and they owe the same source-and-license terms on anything they distribute that incorporates GPL code. Leaving means relicensing their own future work permissively and giving up the guaranteed inflow to everything they have already released under the license.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_developers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_developers, payer).

% Firms that want to incorporate GPL-licensed components into products distributed under proprietary terms. Distributing such a derivative requires releasing its source under the GPL. Their options are complying and disclosing source to competitors, buying a commercial exception where dual licensing exists, reimplementing the functionality from scratch, or designing around the GPL code entirely. If they need the specific code, the choice set is narrow.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Consumer electronics and device manufacturers that ship GPL-licensed firmware or libraries inside physical products. Source disclosure is costly to their product model and their noncompliance is visible in shipped devices, which makes them the most frequent enforcement targets. Their paths are compliance settlements, disclosure, or litigation; the obligation follows the shipped product wherever it is sold.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, embedded_device_vendors, payer,
    organized, biographical, constrained, global).

% Holds copyrights on core GNU projects, publishes and maintains the license texts, and issues authoritative interpretation through its licensing team and FAQ. Its institutional purpose is bound up with the license's continued operation; abandoning copyleft would dissolve the mission the organization exists to advance. It initiated the GPLv3 revision to close gaps its own analysis identified, including hardware locking and network-use loopholes.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_foundation, agenda_setter,
    institutional, generational, identity_locked, global).

% Enforces copyleft licenses on behalf of member projects, negotiating compliance and litigating when negotiation fails, as in the BusyBox suits and the Vizio case. Enforcement activity sustains its funding and public standing; defendants' settlement-linked donations and compliance agreements flow through it. Its docket is the license's practical reach.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_conservancy, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_conservancy, beneficiary).

% Companies that publish flagship products under the GPL while selling commercial licenses to the same code. The license's bar on proprietary derivatives is their sales mechanism: customers who need proprietary distribution must purchase the commercial exception. They bear the license terms on the free side of their own offering and collect revenue on the commercial side.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, dual_licensing_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Large operators that run GPL-licensed software on their own servers without distributing it. Under GPLv2 the obligation attaches to distribution, and offering a service over a network is not distribution, so the source-disclosure requirement never attaches to them. They receive the full benefit of the shared code, including improvements their own engineers contribute back voluntarily, without the burden that distributors carry.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, server_side_deployers, beneficiary,
    institutional, biographical, arbitrage, global).

% Developers and firms in the MIT, BSD, and Apache tradition who hold that freedom includes the freedom to make proprietary derivatives, and that a license should not dictate what downstream users do with code. The GPL's terms do not bind them and they are outside its operation, but they contest its central claim in every licensing debate, and their licenses are the main alternative supply of collaboratively built code.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, permissive_license_advocates, excluded,
    organized, biographical, mobile, global).

% National courts that adjudicate enforcement actions: German regional courts upheld GPL enforceability early, while United States litigation over plaintiff standing continues. They do not administer the license; they rule on whether and how its terms bind, and their doctrines shape what the source-disclosure demand can legally require.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyright_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the appropriation problem in collaboratively built software: it guarantees that improvements to shared code remain shared, so contributors can invest work without fearing that a proprietary derivative will enclose the result. It converts many individual contributions into a durable commons by making reciprocity a condition of distribution.
% TRANSFER_FUNCTION: Moves source-code disclosure and license reciprocity from derivative distributors (proprietary integrators, embedded device vendors) to the public at large; moves foreclosed proprietary-option value away from integrators, a slice of which converts into commercial-license revenue where dual licensing exists; moves settlement and compliance funding toward enforcement organizations.
% ABSENT_VOICES: Permissive-license advocates (authored as an excluded stakeholder) hold that freedom includes the freedom to make proprietary derivatives and would object to the license's bar on that path. End users of noncompliant embedded devices hold products whose source was never offered and would object if they knew. Proprietary integrators appear in the arrangement's operation mainly as enforcement defendants rather than as consulted parties.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight, proprietary capture of GPL-derived code becomes lawful, the enforcement ecosystem (SFC's docket, the settlement economy, compliance programs at every large firm) dissolves within years, contribution incentives restructure toward permissive or closed models, and firms that architected products around the guarantee of source availability lose it. The software commons would reorganize around whichever license regimes absorbed the projects.
% FOUNDING_PROBLEM: In the early 1980s, software that research communities had freely shared was being locked down by proprietary vendors: the Xerox printer driver episode, fragmented proprietary Unix systems, users unable to fix or study their own tools. Copyleft was built to make software freedom self-perpetuating — requiring that redistributed derivatives carry the same freedoms, so that generosity could not be converted into enclosure by the first commercial actor to arrive.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the enforcement docket itself (BusyBox litigation, SFC v. Vizio, gpl-violations.org settlements) documents continuing capture attempts by parties who gain nothing from the freedom framing; contemporaneous accounts of proprietary Unix fragmentation and the 1983 GNU announcement corroborate the founding conditions; academic commons scholarship treats appropriation of shared digital goods as an ongoing structural problem. No source outside the beneficiary set attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.38 from this reading's seat: the take from integrators is real (foreclosed proprietary products, forced source disclosure) but the reading assesses it as the price of the guarantee, and the guarantee's beneficiaries are the entire downstream public. Suppression is authored at 0.75 as a raw structural property (it is NOT scaled by power or scope — the engine owns that arithmetic): alternative licensing arrangements for GPL derivatives are foreclosed as a matter of copyright law, which matches the expected structural delta of high suppression of alternative licensing. Theater is low (0.18): enforcement is functional — real litigation, real settlements, real compliance programs — with a modest and recently slightly receded layer of compliance ritual (boilerplate offer-for-source pages, audit theater at large firms). Accessibility_collapse is moderate (0.48): the proprietary-derivative path for a given piece of GPL code collapses completely once the terms are understood, but alternatives persist — permissively licensed equivalents, commercial dual-license purchase, reimplementation, and server-side deployment that the GPLv2 trigger never reaches. Resistance is substantial (0.62): litigation over enforceability and standing, the historical viral/cancer corporate campaign, migration of some projects to permissive licenses, and systematic use of the network-deployment gap. The measurement series run on ONE shared grid (T=0,6,12,18,24,30,36) with every tracked metric authored at every point. The rising suppression_requirement series is deliberate: the story's enforcement history is one of machinery maturing from an untested copyright threat (1989) through the SCO-era fear peak, the gpl-violations.org and BusyBox dockets, to the Vizio standing dispute — enforcement intensification is the dynamic being traced, so the series is authored rather than left to the scalar. The base_extractiveness series rises through the ecosystem-expansion era, peaks around GPLv3, then eases slightly as compliance normalizes into a budgeted cost of doing business.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the data. From the free_software_foundation seat the arrangement is a freedom guarantee it administers and is institutionally fused with (identity_locked exit — it cannot abandon copyleft without dissolving its mission). From the proprietary_integrator and embedded_device_vendor seats the same terms operate as a legal bar on their product models, with constrained exit: comply, buy an exception, reimplement, or design around. From the downstream_users seat the arrangement is an unpriced guarantee they never negotiated. From the server_side_deployer seat it is nearly costless infrastructure — the arbitrage seat experiences the lightest version of a structure whose heaviest version lands on embedded vendors. Note on coalition: the payer seats do not coalition effectively — integrators compete with one another and each prefers to be the exempted one, and dual-licensing vendors actively profit from the bar their fellow payers face, which structurally fragments any payer-side collective action.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for downstream_users, dual_licensing_vendors, and server_side_deployers; victim declarations drive high directionality for proprietary_integrators and embedded_device_vendors, amplified by their constrained exit. free_software_developers are genuinely dual-positioned (beneficiary with secondary payer role): they receive the improvement inflow AND owe reciprocity on their own distributions, so their derived directionality should sit off the pure-beneficiary end — this is the same-person-both-sides structure the secondary_role field exists to carry. server_side_deployers sit nearest the beneficiary end (arbitrage-grade exit) even though this reading itself regards their position as the guarantee's largest leak — the reading's complaint about them is a coverage gap, not a claim that they bear extraction. Receipt surface: gain_flow is authored 'diffuse' as an affirmative checked claim — the dominant flow of what the obligation takes (disclosed derivative source) goes to the public commons and to no named seat; the named side-channels (settlement-linked funding through software_freedom_conservancy, commercial-exception revenue through dual_licensing_vendors) are real but conditional and peripheral to the standing arrangement's core operation, and neither captures the dominant flow. fixing_cost is 'prohibitive': removal would require relicensing, which needs the consent of every copyright contributor to a project — practically impossible for mature codebases — and no seat exists that both could fix the arrangement and would benefit from fixing it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Against pure-extraction readings (the restriction sibling's pull toward snare): the coordination function is genuine and primary — the appropriation problem the arrangement solves is real, its beneficiaries are the entire downstream public rather than a capturing seat, and the receipt surface is diffuse, which is structurally inconsistent with a snare's concentrated capturer. Against pure-coordination readings (the pull toward rope): integrators bear asymmetric costs through the very same license terms that coordinate users, enforcement is active, and the suppression of the proprietary-derivative alternative is high — that is the tangled_rope signature, and this reading authors it honestly rather than claiming its own preferred rope. The founding problem is live, not dead: capture attempts continue on new fronts (embedded firmware violations, server-side deployment outside the GPLv2 trigger, proprietary training on copyleft corpora), so no mandatrophy resolution is declared. The R5 mismatch consumer reads status=live against verdict=world_rearranges — a consistent pairing: the arrangement persists because its problem persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_referent_ambiguity,
    'This story instantiates one reading (copyleft_as_freedom_reading) of the gpl_reciprocity_obligation kernel; which of the three readings — freedom preservation (this), commons anti-enclosure (copyleft_as_commons_reading), or business-model restriction (copyleft_as_restriction_reading) — correctly characterizes the obligation''s primary function?',
    'Cross-reading seat-classification comparison across the three sibling stories: the disagreement is located in the weighting of the beneficiary structure — whose position (end users, the shared code pool, or proprietary integrators) is the moral referent of the arrangement.',
    'If the restriction reading is adopted as primary, epsilon rises substantially and the computed type shifts toward snare; if the commons reading, epsilon stays moderate but the beneficiary emphasis moves from individual users to the pool. This story''s epsilon (0.38) is authored from the freedom seat only and is not valid for the sibling seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_referent_ambiguity, conceptual, 'Which reading of the GPL reciprocity kernel is primary; committer structure routed here per the kernel-reading rules.').

omega_variable(
    consent_vs_imposed_suppression,
    'Is the measured suppression of alternative licensing structural (copyright law forecloses proprietary derivatives whether or not the integrator accepts the framework) or consensual (integrators accepted the terms by choosing to use the code, so enforcement operates contract-like rather than coercively)?',
    'Legal-theoretical analysis of GPL enforceability (license-versus-contract doctrines, the Vizio plaintiff-standing dispute) plus behavioral evidence from firms that claim they never accepted the terms.',
    'If consensual, effective suppression drops sharply and the arrangement reads closer to ordinary contract coordination; if imposed, suppression stands as measured and the extraction asymmetry carries more weight in the type computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_imposed_suppression, conceptual, 'Whether the license binds by consent or by imposed legal structure.').

omega_variable(
    freedom_referent_disagreement,
    'Does user freedom include the downstream developer''s freedom to make proprietary derivatives (the BSD position), or only the end user''s four freedoms — such that suppressing proprietary integration protects freedom rather than violating it?',
    'Conceptual analysis of whose liberty the license maximizes and what trade it imposes; this is the exact axis on which this reading separates from copyleft_as_restriction_reading.',
    'If integrator freedom counts, the suppression metric measures freedom-violation rather than freedom-protection and the computed type shifts toward snare; if not, suppression is the enforcement of the guarantee itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_referent_disagreement, conceptual, 'The definition of freedom on which the sibling readings diverge.').

omega_variable(
    saas_loophole_coverage,
    'Does the distribution-triggered reciprocity obligation actually cover the dominant modern capture vector — server-side deployment — or does the freedom guarantee leak exactly where integration pressure is largest?',
    'Measure the share of GPL-licensed code running as network services without source obligations, and AGPL adoption rates among projects exposed to that vector.',
    'If the leak is large, the arrangement''s suppression is high where it binds but the guarantee is partial in practice; the reading''s own success claim weakens even though the measured metrics do not move.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(saas_loophole_coverage, empirical, 'Whether the GPLv2 network-use gap undermines the freedom guarantee''s real-world coverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_freedom_reading_tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gpl_freedom_reading_tr_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(gpl_freedom_reading_tr_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(gpl_freedom_reading_tr_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(gpl_freedom_reading_tr_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(gpl_freedom_reading_tr_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(gpl_freedom_reading_tr_t36, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 36, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl_freedom_reading_be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl_freedom_reading_be_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(gpl_freedom_reading_be_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(gpl_freedom_reading_be_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(gpl_freedom_reading_be_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(gpl_freedom_reading_be_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(gpl_freedom_reading_be_t36, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 36, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl_freedom_reading_su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gpl_freedom_reading_su_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(gpl_freedom_reading_su_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(gpl_freedom_reading_su_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(gpl_freedom_reading_su_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(gpl_freedom_reading_su_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(gpl_freedom_reading_su_t36, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 36, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the GPL's viral licensing' covers three structurally distinct claims about one kernel text and is decomposed into a three-story constraint family per the epsilon-invariance principle: copyleft_as_freedom_reading (this story — epsilon 0.38, the integrator burden assessed as legitimate freedom enforcement), copyleft_as_commons_reading (epsilon moderate, referent the shared code pool and enclosure dynamics), and copyleft_as_restriction_reading (epsilon substantially higher, the identical burden assessed as pure business-model restriction). Same arrangement, different reading seats, different epsilon — one story per reading, linked by affects_constraints. The upstream reading (this one, historically and institutionally prior through the FSF) shapes the operating environment of the downstream commons reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
