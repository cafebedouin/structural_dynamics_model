% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: GPL Reciprocity Obligation (Copyleft-as-Restriction Reading)
 *   domain: economic/legal-technological (software licensing / intellectual property / open source governance)
 *
 * SUMMARY:
 *   A fixed license text — the GNU General Public License's reciprocity
 *   clause — requires that anyone distributing modified or incorporated
 *   copies of licensed code pass the same source-disclosure obligations
 *   downstream. Read through the business-model lens this story instantiates,
 *   the clause operates as a boundary on commercial integration: a firm
 *   building a product on licensed components must publish its own source,
 *   purchase a proprietary-use exemption where a copyright holder sells one,
 *   or engineer around the component. The story tracks who that boundary
 *   actually channels value toward as the software economy scaled:
 *   exemption-fee collectors, service operators delivering licensed code
 *   outside the distribution trigger, and operators of proprietary
 *   derivatives occupying the spaces enforcement reaches slowly — while the
 *   reciprocal-return promise that motivates contributors thins and unpaid
 *   maintainers absorb the difference. Time points are years since the
 *   license version that fixed the modern text (t0 = 1991, t34 = 2025). This
 *   file instantiates one reading of a contested kernel; the family
 *   decomposition is recorded in network.dual_formulation_note and
 *   commentary.kernel_context. KEY AGENTS (by structural relationship): -
 *   dual_licensing_vendors: Primary beneficiary (powerful/mobile) — sells the
 *   proprietary-use exemptions the boundary's strictness creates -
 *   hyperscale_cloud_operators: Structural beneficiary
 *   (institutional/arbitrage) — consumes licensed infrastructure through the
 *   service path the distribution trigger does not reach -
 *   proprietary_fork_operators: Secondary beneficiary (powerful/arbitrage) —
 *   ships proprietary derivatives from enforcement's slow zones -
 *   commons_code_contributors: Primary target (powerless/constrained) — the
 *   promised return flow thins as corporate consumption routes around
 *   obligations - volunteer_maintainers: Primary target
 *   (powerless/identity_locked) — absorbs the maintenance burden others
 *   monetize - integration_seeking_isvs: Target (moderate/constrained) —
 *   faces disclose, pay, or engineer around on every component decision -
 *   fsf_and_gpl_copyright_holders: Agenda setter
 *   (institutional/identity_locked) — administers the text, its
 *   interpretation, and its enforcement - downstream_end_users: Incidental
 *   beneficiary (organized/mobile) — receives working foundations at zero
 *   license cost - burned_out_former_maintainers: Excluded voice
 *   (powerless/mobile) — exited with the sharpest testimony and no seat -
 *   open_source_governance_scholars: Analytical observer
 *   (analytical/analytical) — sees the full structure across decades
 *
 * KEY AGENTS:
 *   - dual_licensing_vendors: Primary beneficiary (powerful/mobile) — collects exemption fees created by the boundary's strictness
 *   - hyperscale_cloud_operators: Structural beneficiary (institutional/arbitrage) — consumes licensed infrastructure through the service-delivery path
 *   - proprietary_fork_operators: Secondary beneficiary (powerful/arbitrage) — runs proprietary derivatives in enforcement's slow zones
 *   - commons_code_contributors: Primary target (powerless/constrained) — the reciprocal-return promise thins as corporate consumption routes around obligations
 *   - volunteer_maintainers: Primary target (powerless/identity_locked) — absorbs the maintenance burden others monetize
 *   - integration_seeking_isvs: Target (moderate/constrained) — faces disclose, pay, or engineer around on every component decision
 *   - fsf_and_gpl_copyright_holders: Agenda setter (institutional/identity_locked) — administers the text and its interpretation
 *   - downstream_end_users: Incidental beneficiary (organized/mobile) — receives working foundations at zero license cost
 *   - burned_out_former_maintainers: Excluded voice (powerless/mobile) — exited with the sharpest testimony and no seat
 *   - open_source_governance_scholars: Analytical observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.68).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.55).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation (Copyleft-as-Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "economic/legal-technological (software licensing / intellectual property / open source governance)").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '71b3e49c-049f-4c6d-8886-4a9063d8579b').
narrative_ontology:cs_kernel_codification('71b3e49c-049f-4c6d-8886-4a9063d8579b', fixed_text).
narrative_ontology:cs_authority_grounding('71b3e49c-049f-4c6d-8886-4a9063d8579b', lineage).
narrative_ontology:cs_interpretation_layer_present('71b3e49c-049f-4c6d-8886-4a9063d8579b').
narrative_ontology:cs_reading_relation('71b3e49c-049f-4c6d-8886-4a9063d8579b', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('71b3e49c-049f-4c6d-8886-4a9063d8579b', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('71b3e49c-049f-4c6d-8886-4a9063d8579b', foundational, viral_licensing_prohibits_proprietary_integration).
narrative_ontology:cs_axiom_status(viral_licensing_prohibits_proprietary_integration, holdable).
narrative_ontology:cs_axiom_grounding('71b3e49c-049f-4c6d-8886-4a9063d8579b', viral_licensing_prohibits_proprietary_integration, empirically_contingent).
narrative_ontology:cs_axiom('71b3e49c-049f-4c6d-8886-4a9063d8579b', foundational, proprietary_integration_is_legitimate_commerce).
narrative_ontology:cs_axiom_status(proprietary_integration_is_legitimate_commerce, holdable).
narrative_ontology:cs_axiom_grounding('71b3e49c-049f-4c6d-8886-4a9063d8579b', proprietary_integration_is_legitimate_commerce, instrumental).
narrative_ontology:cs_reference_frame('71b3e49c-049f-4c6d-8886-4a9063d8579b', reciprocity_as_contractual_boundary).
narrative_ontology:cs_drift_state('71b3e49c-049f-4c6d-8886-4a9063d8579b', contemporary_cloud_saas_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('71b3e49c-049f-4c6d-8886-4a9063d8579b', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_vendors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, hyperscale_cloud_operators).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_fork_operators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_code_contributors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, volunteer_maintainers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, integration_seeking_isvs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, downstream_end_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, hyperscale_cloud_operators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, license_textualism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and maintains the license texts, runs compliance and enforcement programs, adjudicates what counts as a derivative work, and defines the interpretive tradition through FAQs and licensing counsel. Its authority rests on custodianship of the founding text; changing course would require re-founding its own legitimacy, so it administers rather than exits.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, fsf_and_gpl_copyright_holders, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold copyrights on widely deployed licensed components — database engines, interface toolkits — and sell proprietary-use exemptions alongside the public-license option. The strictness of the public terms is what makes the paid exemption valuable; revenue scales with how costly compliance-without-payment is. Holding the copyrights, they can relicense or pivot at will.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Run licensed infrastructure — kernels, toolchains, databases — at planetary scale delivered as services. Because service delivery triggers no distribution obligation under the classic text, they consume the commons' output with minimal mandated return, contributing selectively where contribution buys influence. Scale and legal staff make enforcement outcomes tolerable; multi-region redundancy gives them exit options no smaller actor has.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, hyperscale_cloud_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, hyperscale_cloud_operators, payer).

% Ship proprietary derivatives of licensed ecosystems: out-of-tree kernel modules behind binary loaders, forked productivity suites sold under commercial terms after acquisition, appliance firmware built on licensed bootloaders with sources published late or partially. Each operates where enforcement arrives slowly or not at all, and each business plan prices in the risk of a compliance demand.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_fork_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% Contribute patches and features expecting the reciprocal-return promise: improvements flow back to everyone, including themselves. When corporate consumers take without returning — via service delivery, forks, or quiet non-compliance — the promised return flow thins, and contributors find their gift funding proprietary products they cannot use in their own commercial work without accepting the same disclosure terms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_code_contributors, payer,
    powerless, biographical, constrained, global).

% Carry security patching, review, and release management for projects consumed by large firms. Unpaid labor sustains infrastructure others monetize; burnout and abandonment are the recurring endpoints. Walking away means abandoning a project that is also their reputation and community standing, so exit is socially expensive even when the burden is unsustainable.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, volunteer_maintainers, payer,
    powerless, immediate, identity_locked, global).

% Build commercial products that would naturally incorporate licensed components — a database, a media library, a compiler toolchain. The terms force a three-way choice: release their own source, buy an exemption where one is sold, or engineer around the component at real cost. Larger rivals spread the overhead over more revenue; for a mid-size firm it is a genuine levy on every architecture decision.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, integration_seeking_isvs, payer,
    moderate, biographical, constrained, global).

% Receive working software at zero license cost — servers, phones, desktops, and appliances all run on licensed foundations. They neither enforce nor negotiate the terms; their interest is continued availability and security, exercised collectively through attention and bug reports. Most cannot name the license governing their device firmware.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, downstream_end_users, beneficiary,
    organized, biographical, mobile, global).

% Former stewards who abandoned projects after years of unpaid maintenance for corporate consumers. They hold the sharpest testimony about what the unkept return promise costs, but they are out of the rooms where license strategy, funding, and enforcement are decided, and nothing in the arrangement recalls them.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, burned_out_former_maintainers, excluded,
    powerless, immediate, mobile, global).

% Study license adoption, contribution flows, and enforcement outcomes across decades; document where reciprocity holds and where it leaks. They take testimony from every seat and publish analyses that shape how courts, firms, and foundations read the text.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_governance_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_vendors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in jointly produced code: anyone may use, study, modify, and redistribute the licensed work, but anyone distributing derivatives must extend the same rights and source availability downstream, making improvement flows reciprocal rather than voluntary.
% TRANSFER_FUNCTION: Moves source disclosures, improvement work, and exemption payments: the license terms move code and modifications from integrating firms toward the public commons; dual-licensing copyright holders move proprietary-use rights to firms in exchange for payment; in practice, uncompensated maintenance burden moves onto volunteer maintainers, and unreciprocated infrastructure value moves toward service operators.
% ABSENT_VOICES: Burned-out former maintainers hold the sharpest testimony about what the unkept return promise costs and are absent precisely because the burden removed them; proprietary fork operators and service-only consumers benefit from the enforcement gaps and are absent from every conversation about closing them; end users of embedded derivatives cannot object because firmware provenance is invisible to them.
% DISAPPEARANCE_RATIONALE: Corporate consumers would absorb prominent licensed codebases into proprietary products within quarters; exemption-fee revenue models would collapse immediately; contribution flows would reroute toward permissive licenses or decline as enclosure proceeds; the deployed base of phones, servers, and appliances built on the licensed foundations would keep running but its shared-upgrade pathway would fragment into private forks — the software economy rearranges around whatever equilibrium follows.
% FOUNDING_PROBLEM: In the early 1980s a laboratory printer lacked usable driver source, and proprietary Unix vendors routinely took shared code proprietary, cutting off the improvements communities had contributed. The license was built to make cooperation self-enforcing: every redistribution carries the obligation forward, so taking the code and improving it entails giving the improvements back.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: corporate engineering histories record pre-license code-sharing losses; academic work on free-riding in jointly produced infrastructure documents the mechanism; and the continuing attempts of service operators and device vendors to appropriate licensed codebases without reciprocity presuppose that the enclosure problem persists. Exemption-fee vendors also assert the problem is live, but they monetize it, so their attestation is discounted; the independent seats carry the claim.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.68 is substantial but concentrated: the boundary binds mid-size integrating firms hard, binds scaled operators lightly (service delivery sits outside the distribution trigger; enforcement reaches them slowly), and lands its residual cost on contributors and maintainers whose promised returns thin. Suppression 0.55 is structural, not internalized: the license conditions reuse on reciprocity, and corporate procurement norms convert that condition into a chilling effect on adoption inside proprietary codebases; suppression enters the engine as a raw property, unscaled by power or scope. Theater 0.35: compliance programs, enforcement counsel, and foundation campaigns do recover real code and real compliance, but a growing share of activity is rhetorical defense of the text rather than recovery of reciprocity. Accessibility collapse 0.45: alternatives persist everywhere — permissive-licensed equivalents, paid exemptions, service-delivery routing, fully proprietary stacks — so the boundary taxes a path rather than eliminating alternatives. Resistance 0.60: procurement bans, permissive-license migration of whole ecosystems, litigation resistance, and the critical discourse this reading itself exemplifies. The temporal series share one seven-point grid: extractiveness accumulates monotonically as corporate consumption scaled faster than reciprocity; enforcement capacity (suppression_requirement) builds steeply through the compliance-lab and foundation era and plateaus after t22 while extraction keeps rising — the signature the lifecycle watcher treats as coordination drifting toward extraction. No oscillation is modeled; the record over this interval is monotonic. Coalition note: the payer seats are individually powerless but aggregable (maintenance funds, contributor associations, integrating-firm trade groups); the series assumes no such coalition formed during the interval, which the maintainer-burnout record supports. Claim and metrics are authored independently: the tangled_rope claim reflects the judgment that a real coordination function and a real asymmetric receipt coexist in one structure; the metrics describe operation as observed.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience one instrument incompatibly. The exemption vendor experiences a revenue engine it built; the integrating firm experiences a levy on every architecture decision; the maintainer experiences an unkept promise consuming unpaid capacity; the foundation experiences stewardship of a founding text. The engine computes per-seat classifications from the structural data rather than averaging them. Inter-institutional divergence: the foundation and the hyperscale operators hold comparable institutional power with opposite exit positions — the foundation cannot leave its own text (identity-locked custodianship), the operators can leave any given codebase (arbitrage) — so equal nominal standing yields opposite directionalities. Same-level lateral divergence: the exemption vendor and the fork operator are both powerful and mobile yet relate oppositely to the text, one monetizing its strictness, the other surviving around it; role declarations, not power atoms, carry that difference. Identity lock operates on two seats: maintainers fuse relational identity with project stewardship, so exit means abandoning their creation and community standing; the foundation fuses institutional identity with custody of the text, so revision means re-founding its own authority. If maintainer stewardship became compensated and portable, the supply-side burden would reprice and the extraction profile would flatten.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations order the seats and the derivation chain needs no overrides. Three proprietary-vendor seats sit near the beneficiary end: exemption-fee collectors (the boundary's strictness is their pricing power), service operators consuming through the path the distribution trigger does not reach, and fork operators occupying spaces enforcement reaches slowly — mobile and arbitrage exit damp their effective burden toward subsidy. Three payer seats sit near the target end: contributors whose promised returns thin, maintainers absorbing the residual burden, and integrating firms choosing among disclosing source, paying for exemption, or engineering around — constrained and identity-locked exit push them toward the full-target end. End users sit near symmetric: broad benefit from free foundations, diffuse and indirect exposure to the boundary's costs. The agenda setter is administered through its role rather than the beneficiary derivation. Only extractiveness is scaled (by directionality and the license's global scope, which raises verification difficulty); suppression stays raw.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — code taken without return fencing off shared improvement — is still live, and the arrangement still performs its coordinating function at kernel scale. The classification risk runs in both directions and the tangled_rope claim holds the middle: a rope reading would credit the coordination and miss that the boundary's value concentrates as exemption revenue while scaled consumers route around the trigger; a snare reading would erase the demonstrable coordination (cross-corporate kernel collaboration exists because reciprocity is enforced). The temporal series guards the drift: enforcement capacity plateaus after t22 while extraction continues rising, which is the accumulation signature separating a stable hybrid from a decaying one. If a later interval showed theater overtaking function and receipts concentrating further, the honest recomputation moves toward snare; if service-path closure restored return flows, it moves back toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates one reading (copyleft_as_restriction_reading) of the contested kernel gpl_reciprocity_obligation; how would instantiating a sibling reading change the structural data?',
    'Author the sibling stories (copyleft_as_freedom_reading, copyleft_as_commons_reading) against the same referent and compare computed per-seat classifications; the disagreement is located in who the license''s operation channels value toward and whether the reciprocity clause protects or burdens.',
    'The freedom reading moves the beneficiary seat to end users and the victim seat to proprietary captors; the commons reading moves the beneficiary seat to the contributor commons and the victim seat to would-be enclosers. Either swap can move the computed type from tangled_rope toward rope; this story''s structural data is valid only for its own seat assignments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three rival readings of the GPL reciprocity kernel.').

omega_variable(
    saas_loophole_extraction_attribution,
    'Does service delivery outside the distribution trigger leave the largest consumers of licensed code outside the obligation, so that the boundary''s cost concentrates on smaller integrating firms and on contributors rather than on scaled operators?',
    'Track reciprocal-license adoption in server-side ecosystems and the revenue mix of licensed infrastructure (distribution-embedded versus service-delivered); compare return-flow rates across delivery modes.',
    'If the service path dominates, this reading''s core claim (the license prohibits proprietary integration) is empirically hollow for the biggest actors, the boundary''s cost concentrates on marginal integrators, and the asymmetry sharpens toward snare-like structure; if updated licenses close the path, costs spread more evenly and the tangled_rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_loophole_extraction_attribution, empirical, 'Whether the distribution trigger leaves scaled service operators outside the obligation.').

omega_variable(
    enforcement_function_vs_receipt,
    'Is license enforcement primarily recovering code and compliance for the commons, or generating settlements and selective deterrence whose receipts bypass the contributors who supplied the code?',
    'Audit enforcement outcomes: destination of settlements and restitution, size distribution of targeted violators, and whether recovered sources reach the contributor community.',
    'Functional recovery supports the coordination half of the tangled_rope reading; settlement-centered enforcement that spares scaled violators supports a snare-leaning recomputation and elevates dual_licensing_vendors as the capture seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_function_vs_receipt, empirical, 'Where enforcement receipts land relative to contributor supply.').

omega_variable(
    agenda_setter_identity_lock,
    'Is the license''s administrative seat (foundation custodians and enforcing copyright holders) able to revise strategy in response to demonstrated leakage, or is it locked into defending the text as written?',
    'Observe institutional responses to demonstrated loophole exploitation and failed enforcement: post-dismissal behavior in the long-running kernel litigation line, uptake of reciprocal-license variants, and license-version migration campaigns.',
    'Lock freezes the reference frame while practice drifts, hardening a piton-or-worse trajectory for the administered text; demonstrated revisability keeps the tangled_rope classification stable and correctable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agenda_setter_identity_lock, conceptual, 'Revisability of the interpretive authority under evidence of drift.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the commitment-system kernel the license text itself (fixed text with lineage authority) or the distributed interpretive practice of license files, metadata conventions, and community custom (distributed authority with no designated interpreter)?',
    'Test whether enforcement and compliance outcomes track the canonical text and its designated interpreters, or track decentralized convention that would persist if the foundations dissolved.',
    'Under the distributed framing, agenda-setting power disperses, the lineage authority claim weakens, and coupling analysis loses its concentrated interpreter node; the fixed-text framing was chosen because designated interpreters (foundation licensing teams, enforcement counsel) demonstrably adjudicate hard cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Two coherent framings of what the kernel is; the classification differs between them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t6, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement_basis(gpl__tr_t6, observed).
narrative_ontology:measurement(gpl__tr_t11, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 11, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t11, observed).
narrative_ontology:measurement(gpl__tr_t17, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 17, 0.24).
narrative_ontology:measurement_basis(gpl__tr_t17, observed).
narrative_ontology:measurement(gpl__tr_t22, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 22, 0.29).
narrative_ontology:measurement_basis(gpl__tr_t22, observed).
narrative_ontology:measurement(gpl__tr_t28, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 28, 0.33).
narrative_ontology:measurement_basis(gpl__tr_t28, observed).
narrative_ontology:measurement(gpl__tr_t34, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 34, 0.35).
narrative_ontology:measurement_basis(gpl__tr_t34, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t6, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement_basis(gpl__be_t6, observed).
narrative_ontology:measurement(gpl__be_t11, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 11, 0.47).
narrative_ontology:measurement_basis(gpl__be_t11, observed).
narrative_ontology:measurement(gpl__be_t17, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 17, 0.55).
narrative_ontology:measurement_basis(gpl__be_t17, observed).
narrative_ontology:measurement(gpl__be_t22, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 22, 0.61).
narrative_ontology:measurement_basis(gpl__be_t22, observed).
narrative_ontology:measurement(gpl__be_t28, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 28, 0.65).
narrative_ontology:measurement_basis(gpl__be_t28, observed).
narrative_ontology:measurement(gpl__be_t34, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 34, 0.68).
narrative_ontology:measurement_basis(gpl__be_t34, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t6, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 6, 0.26).
narrative_ontology:measurement_basis(gpl__su_t6, observed).
narrative_ontology:measurement(gpl__su_t11, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 11, 0.36).
narrative_ontology:measurement_basis(gpl__su_t11, observed).
narrative_ontology:measurement(gpl__su_t17, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 17, 0.46).
narrative_ontology:measurement_basis(gpl__su_t17, observed).
narrative_ontology:measurement(gpl__su_t22, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 22, 0.51).
narrative_ontology:measurement_basis(gpl__su_t22, observed).
narrative_ontology:measurement(gpl__su_t28, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 28, 0.54).
narrative_ontology:measurement_basis(gpl__su_t28, observed).
narrative_ontology:measurement(gpl__su_t34, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 34, 0.55).
narrative_ontology:measurement_basis(gpl__su_t34, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'viral licensing / copyleft' decomposes into three structurally distinct constraints per the epsilon-invariance principle: measuring the license as a user-freedom protector, as an anti-enclosure commons technology, and as a business-model restriction yields different epsilon values, different beneficiary/victim sets, and different classifications. They are separate stories linked here. The restriction reading is downstream of the other two in discourse: critics cite the license's observed effects on commercial integration to argue against the freedom and commons framings, so this story's operation pressures its siblings' adoption environment without resolving the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
