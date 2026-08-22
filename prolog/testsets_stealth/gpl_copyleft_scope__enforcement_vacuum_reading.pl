% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope — Enforcement-Vacuum Reading (Licensed Interpretive Plurality)
 *   domain: legal/technological (software licensing, intellectual property, open source governance)
 *
 * SUMMARY:
 *   GPL Section 2(b) extends the license to 'the work as a whole' but never
 *   defines which forms of code coupling create that whole, and three decades
 *   of practice have produced no definitive judicial resolution: the SFC v.
 *   Vizio action was dismissed on standing, adjacent disputes settled, and
 *   the question remains open. This story instantiates the
 *   enforcement_vacuum_reading of the gpl_copyleft_scope kernel: the
 *   operative arrangement is not either reading of the license but the
 *   licensed plurality itself — a regime in which both readings remain live
 *   and the binding scope on any adopter is set by which interpretive
 *   community holds enforcement capacity in their context. Assessed by this
 *   reading's own lights, the arrangement is a hybrid: it performs genuine
 *   coordination (a single code commons serves two ecologies that no
 *   definitive ruling would simultaneously accommodate, with distributed
 *   enforcement capacity supplying reciprocity) while imposing asymmetric
 *   costs (an ambiguity tax on adopters who need certainty, uncompensated
 *   compliance costs on strict-compliance projects, exposed positions for
 *   small integrators) from which pragmatic adopters, the enforcement
 *   community, dual-licensing vendors, and compliance service providers draw
 *   benefits. The claim and the metrics are authored independently: the
 *   claimed type states what the structure appears to be from this reading's
 *   seat, and the metrics describe the arrangement's observed operation —
 *   where the engine's per-seat computations diverge from the claim, that
 *   divergence is the datum. Sibling readings (strong_copyleft_reading,
 *   narrow_scope_reading) are separate constraint stories with their own
 *   epsilon values, linked via network.affects_constraints; their identifiers
 *   here follow this story's naming convention. KEY AGENTS (by structural
 *   relationship): - fsf_enforcement_community: Agenda-setting beneficiary
 *   (organized/identity_locked) — authors and interprets the license text;
 *   its enforcement posture keeps the strong reading live; its leverage
 *   depends on the plurality persisting - industry_ecosystem_governors:
 *   Agenda-setting beneficiary (institutional/arbitrage) — set ecosystem
 *   norms that operationalize the narrow reading in the highest-volume
 *   contexts - pragmatic_adopters: Primary beneficiary (powerful/constrained)
 *   — incorporate GPL components under narrow-scope practice, converting
 *   ambiguity into flexibility - clarity_seeking_adopters: Primary target
 *   (powerful/constrained) — pay the ambiguity tax in legal opinions,
 *   compliance engineering, and risk reserves - copyleft_purist_projects:
 *   Secondary target (moderate/identity_locked) — bear full strong-reading
 *   compliance costs while practical scope drifts narrow around them -
 *   small_integrators: Diffuse target and excluded voice (powerless/trapped)
 *   — bear the ambiguity's costs with no seat in any interpretive community -
 *   dual_licensing_vendors: Beneficiary (powerful/mobile) — monetize the
 *   ambiguity through proprietary-exception business models -
 *   compliance_service_providers: Beneficiary (organized/mobile) — receive
 *   fee revenue directly for navigating the ambiguity - copyright_judiciary:
 *   Institutional observer — its non-engagement is the vacuum's defining
 *   condition - open_source_governance_analysts: Analytical observer — sees
 *   the full two-ecology structure from outside both beneficiary sets
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.4).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope — Enforcement-Vacuum Reading (Licensed Interpretive Plurality)").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "legal/technological (software licensing, intellectual property, open source governance)").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '52be06c2-0999-4039-8576-d0f7fc0c9f43').
narrative_ontology:cs_kernel_codification('52be06c2-0999-4039-8576-d0f7fc0c9f43', fixed_text).
narrative_ontology:cs_authority_grounding('52be06c2-0999-4039-8576-d0f7fc0c9f43', distributed).
narrative_ontology:cs_reading_relation('52be06c2-0999-4039-8576-d0f7fc0c9f43', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('52be06c2-0999-4039-8576-d0f7fc0c9f43', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('52be06c2-0999-4039-8576-d0f7fc0c9f43', foundational, operative_scope_set_by_enforcement_capacity).
narrative_ontology:cs_axiom_status(operative_scope_set_by_enforcement_capacity, holdable).
narrative_ontology:cs_axiom_grounding('52be06c2-0999-4039-8576-d0f7fc0c9f43', operative_scope_set_by_enforcement_capacity, empirically_contingent).
narrative_ontology:cs_axiom('52be06c2-0999-4039-8576-d0f7fc0c9f43', foundational, interpretive_plurality_is_licensed_law).
narrative_ontology:cs_axiom_status(interpretive_plurality_is_licensed_law, holdable).
narrative_ontology:cs_axiom_grounding('52be06c2-0999-4039-8576-d0f7fc0c9f43', interpretive_plurality_is_licensed_law, conventional).
narrative_ontology:cs_reference_frame('52be06c2-0999-4039-8576-d0f7fc0c9f43', interpretive_plurality_regime).
narrative_ontology:cs_drift_state('52be06c2-0999-4039-8576-d0f7fc0c9f43', post_vizio_standing_dismissal, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('52be06c2-0999-4039-8576-d0f7fc0c9f43', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_enforcement_community).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, dual_licensing_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, compliance_service_providers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, copyleft_purist_projects).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, small_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystem_governors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and maintains the GPL text and publishes interpretive guidance (the license FAQ) asserting that combined and dynamically linked works fall under the license. Brings and supports enforcement actions against non-compliant distributors — the BusyBox wave of the late 2000s, the SFC v. Vizio suit in 2021. Its interpretive position survives because it keeps asserting it; a definitive judicial adoption of the narrow reading would strip its central claim of practical force, and its organizational purpose is constituted by the strong-copyleft project, so it cannot trade its reading away for settlement or certainty. Funding and volunteer energy flow to it in proportion to the perceived threat to that project.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_enforcement_community, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_enforcement_community, beneficiary).

% Run the foundations and vendor consortiums through which most GPL code reaches industry — kernel foundations, mobile-ecosystem governance, corporate open-source programs. They publish compliance guides that operationalize the narrow reading (userspace/kernel separation, syscall-exception notes, license-compatibility matrices), and their practice, at global volume, is the de facto operative scope in most commercial contexts. They fund their own governance infrastructure and can shift projects to permissive licenses or new foundations if community enforcement ever gained teeth, so their commitment to the current arrangement is strategic rather than binding.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystem_governors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystem_governors, beneficiary).

% Companies that incorporate GPL components into commercial products under narrow-scope practice without purchasing definitive legal certainty. They save the cost of the legal opinions, compliance engineering, and license negotiation that a strong-reading world would require, and they capture the option value of the ambiguity: if enforcement ever arrived, they could settle cheaply or re-architect; until then, they keep the flexibility. Their dependence on the components they have shipped limits repositioning, but their exposure is a calculated bet rather than an imposed burden.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    powerful, biographical, constrained, global).

% Vendors that release code under the GPL while selling proprietary-use exceptions — a business model that only functions because the license's reach is uncertain enough that customers will pay to escape it. A definitive narrow ruling would collapse the premium their exceptions command; a definitive strong ruling would collapse their proprietary customer base. The ambiguity is the product. They can relicense or pivot their business models, so their position in the arrangement is mobile.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, dual_licensing_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Enterprises whose deal structures require license certainty — M&A due diligence, regulated industries, enterprise procurement, IPO audits. They pay for outside counsel opinions, compliance tooling, code-scanning regimes, and risk reserves on every transaction involving GPL components, and they cannot obtain certainty at any price: no forum sells a declaratory ruling cheaply, and an adverse precedent would be catastrophic. They need the functionality the GPL code provides, so avoiding it entirely is often unavailable; their exit is constrained to paying.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    powerful, biographical, constrained, global).

% Volunteer and mission-driven projects that treat full compliance under the strong reading as their standard — rejecting permissive relicensing, maintaining strict compatibility policies, sometimes forgoing industry contribution channels that would compromise the reading. They bear the compliance and compatibility costs of the strong reading in full while the arrangement's practical scope drifts narrow around them, and their reciprocity expectations — code taken under the license returns under the license — are diluted without compensation. Their project identity is constituted by the strict practice, so exit would mean becoming a different project.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, copyleft_purist_projects, payer,
    moderate, generational, identity_locked, global).

% Small firms and contract shops that embed GPL components in shipped products without legal departments or compliance budgets. They bear the arrangement's costs in the most exposed position — no seat in either interpretive community, no capacity to fund a declaratory action, products already shipped on top of the components — while receiving little of the flexibility rents that larger pragmatic adopters capture. Their compliance behavior is set by whatever enforcement reaches them, which is to say, by luck.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, small_integrators, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, small_integrators, excluded).

% Law firms, code-scanning vendors, and open-source compliance consultancies that sell navigation of the ambiguity: audits, opinions, remediation, and policy design. Their revenue scales with the uncertainty — a definitive resolution would collapse a market segment — and they are the seat that receives the ambiguity tax in fee form as clarity-seeking adopters and integrators spend on certainty they cannot buy. They can reposition into adjacent governance and security markets, so their position is mobile.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, compliance_service_providers, beneficiary,
    organized, biographical, mobile, global).

% Federal courts, which have never been presented with a clean, fully-briefed case on GPL linking scope: the SFC v. Vizio action was dismissed on standing, adjacent disputes settled, and certiorari in neighboring copyright-boundary questions has been denied. Their non-engagement is the vacuum's defining condition — the forum that could resolve the kernel has structural reasons (settling parties, standing doctrine, docket economics) never to reach it. They collect and pay nothing; their capacity is held in reserve.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, copyright_judiciary, observer,
    institutional, generational, analytical, national).

% Academic and policy analysts who map the two-ecology structure: they document that operative scope tracks enforcement capacity rather than doctrine, track the distribution of enforcement actions, and supply the outside-the-beneficiary attestation that the founding problem is real and unresolved. They hold no stake in either reading.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, open_source_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, compliance_service_providers).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single shared code commons usable by two ecologies with incompatible readings of the same license text: the ambiguity lets copyleft-committed projects and proprietary-adjacent industry stacks draw on the same body of code without a definitive ruling that would force one ecology's practice onto the other. Enforcement capacity, distributed across communities, supplies the reciprocity mechanism that a single authoritative reading would otherwise have to supply.
% TRANSFER_FUNCTION: Moves legal risk and transaction cost from enforcement-capacity holders and ambiguity-tolerant adopters onto clarity-seeking adopters and full-compliance projects: the parties who need certainty pay for legal opinions, compliance engineering, and risk reserves, while the parties comfortable with ambiguity pay little. It also moves de facto rule-making authority from the license text to whichever interpretive community holds enforcement capacity in each context.
% ABSENT_VOICES: Small integrators and downstream users hold no seat in either interpretive community; contributors who wrote code under strong-copyleft expectations have no seat where operative scope gets set; would-be declaratory-judgment plaintiffs lack the resources to create definitive precedent — and the parties with capacity to litigate have structural reasons not to, since a definitive ruling would destroy the flexibility each side currently exploits.
% DISAPPEARANCE_RATIONALE: A definitive judicial resolution overnight would invert compliance postures across one ecology or the other: under a strong ruling, most industry stacks built on GPL components become non-compliant at a stroke; under a narrow ruling, the community enforcement posture loses its object and dual-licensing models reprice. Either way the two-ecology commons reorganizes around the winning reading, and the seats whose positions depend on the plurality (the enforcement community, dual-licensing vendors, compliance service providers) lose their footing.
% FOUNDING_PROBLEM: The GPL's authors needed Section 2(b) to reach combined works without defining 'derivative work' precisely: a precise narrow definition would have surrendered the copyleft's reach over code coupling, while a precise strong definition would have made the license unadoptable by industry. The unsettled boundary — inherited from copyright doctrine's own circuit split — was left open, and the license's reach was preserved by leaving it open.
% FOUNDING_PROBLEM_CORROBORATION: Copyright scholarship documenting the derivative-work circuit split attests that the underlying doctrinal boundary is genuinely unsettled, independent of any party's licensing interest; the LGPL's existence is the license author's own acknowledgment that the strong reading's adoptability cost was real; and judicial avoidance across three decades (settlements, the SFC v. Vizio standing dismissal, certiorari denials in adjacent cases) corroborates from outside the benefiting parties that no forum has been willing or able to settle it. Industry legal memoranda from the late 1990s onward attest the same uncertainty from the adopter side.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the arrangement possesses both a genuine coordination function — a single code commons serving two ecologies with incompatible readings, with distributed enforcement capacity supplying the reciprocity that a single authoritative reading would otherwise have to supply — and asymmetric extraction: the ambiguity tax falls on clarity-seeking adopters, strict-compliance projects, and small integrators while flexibility rents flow to pragmatic adopters, leverage accrues to the enforcement community, and fee revenue flows to compliance service providers. Active enforcement is structurally required: without community enforcement capacity and ecosystem norm maintenance, neither reading binds and the commons' reciprocity collapses. Extractiveness is authored low-to-moderate (0.38): the arrangement's costs are real but bounded — adopting GPL code is voluntary, and the tax is a transaction-cost premium rather than a coerced transfer. Suppression (0.40) is moderate: no forum sells certainty at any price, and enforcement threats are deliberately left untested, but nothing coerces adoption itself. Theater (0.48) is high because both ecologies perform: the community side maintains enforcement rhetoric whose practical reach is far smaller than its announced scope, and the industry side maintains compliance programs that document narrow-scope practice rather than test it. Accessibility collapse (0.30) is low — the arrangement's defining feature is that alternatives persist: avoid GPL code, align with a community, dual-license, or exploit the ambiguity. Resistance (0.50) is moderate and diffuse: clarity-seekers push for precedent, industry resists community enforcement, purist projects contest dilution — but no seat has an interest in resolving the ambiguity, so resistance never concentrates. The three measurement series share one time grid (1991–2025, seven points each). Suppression_requirement is tracked because enforcement-capacity change is this story's central dynamic: the BusyBox-era enforcement wave (2007–2011) built machinery that the post-Vizio standing environment has partially dismantled, while extractiveness peaked in the same window (the SCO-era litigation climate made ambiguity maximally expensive) and has since partially decayed as narrow-scope practice consolidated. The suppression mechanism is predominantly structural (no forum sells declaratory certainty; threats are left untested by design) with an internalized component (community-norm reputational fear drives over-compliance beyond measured legal risk) — routed to the suppression_mechanism_ambiguity omega rather than resolved in the scalar. Fixing cost is prohibitive: the judiciary could resolve the kernel only if presented a clean case no litigant with capacity will bring; the FSF cannot rewrite the license without schisming its own community (GPLv3's attempted clarification left linking scope untouched); industry governors would destroy option value they currently harvest. Every potential fixer's cost of fixing exceeds the benefit it would capture, which is why the vacuum has persisted for three decades.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the fsf_enforcement_community seat the arrangement is a holding action that preserves the copyleft's reach against dilution — the plurality is not a defect but the last line of defense, and the seat's identity-locked exit means it cannot trade the ambiguity for certainty even on favorable terms. From the industry_ecosystem_governors seat the same arrangement is settled practice: narrow scope is simply how linking works, and the community's contrary reading is noise from a shrinking seat. From the clarity_seeking_adopters seat the arrangement is an unresolved liability that consumes legal budget on every M&A audit, enterprise contract, and IPO disclosure. From the copyleft_purist_projects seat it is dilution: compliance costs borne in full while competitors take the narrow path, with reciprocity expectations decaying uncompensated. Same-power divergence is sharpest between clarity-seeking and pragmatic adopters — both powerful, both global, differentiated not by power but by dependence on certainty: M&A pipelines, regulated markets, and enterprise procurement make ambiguity expensive for one and cheap for the other. Inter-institutionally, the FSF and the industry foundations are both agenda-setters at the same nominal governance level whose enforcement capacities — and therefore whose operative readings — dominate in disjoint contexts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place pragmatic_adopters, fsf_enforcement_community, dual_licensing_vendors, and compliance_service_providers at the low-d end: the arrangement subsidizes each — flexibility, retained leverage, monetizable exceptions, fee revenue. Victim declarations place clarity_seeking_adopters, copyleft_purist_projects, and small_integrators at the high-d end; the identity-locked and trapped exits of the purist projects and small integrators push them toward the full-target end, since they cannot reposition even on favorable terms. The dual-role seats matter: the FSF and the industry governors are agenda-setters whose beneficiary position means they set the rules under which they collect — derived directionality should read them as low-d despite their enforcement roles, and the derivation from beneficiary declaration plus exit options captures this without override. The copyright_judiciary sits near symmetric: it neither collects nor pays, and its analytical exit keeps it outside the flow. Compliance service providers warrant particular note: they hold a beneficiary role and are also the seat that literally receives the ambiguity tax in fee form, which is why gain_flow names them rather than declaring the flow diffuse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing code coupling across community boundaries without a definition that would break one ecology — is still live, and the arrangement still performs it, so no zombie flag arises (founding_problem_status=live with disappearance_verdict=world_rearranges is the coherent pair). Mandatrophy risk is nonetheless real and tracked: theater_ratio has risen from 0.15 to roughly 0.5 as enforcement rhetoric and compliance ceremony have outgrown their functional reach, and the post-Vizio decay of community enforcement capacity means a growing share of the arrangement's maintenance is performative. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination would erase the concentrated ambiguity tax on clarity-seekers and small integrators; reading it as pure extraction would erase the genuine dual-ecology commons that neither sibling reading alone would sustain. If a definitive precedent lands, this constraint dissolves into one of its siblings — mandatrophy here is contingent on the kernel's judicial resolution rather than on internal decay, and the precedent_resolution_pathway omega carries that contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates only the enforcement_vacuum_reading of kernel gpl_copyleft_scope — how would the structural data and classification change if the same kernel were instantiated under a sibling reading instead?',
    'Read alongside the sibling stories (strong_copyleft_reading, narrow_scope_reading), which author the same kernel under each reading''s own lights; cross-story comparison of epsilon, beneficiary/victim sets, and computed types is the resolution path.',
    'Under the strong reading the arrangement''s epsilon would be substantially higher (extensive costs imposed on proprietary combiners, with all proprietary integrators as victims); under the narrow reading epsilon would be near zero (minimal binding force). The low-to-moderate tangled_rope profile is specific to the enforcement-vacuum referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Reading-commitment scoping: this classification is valid for the enforcement_vacuum_reading referent only.').

omega_variable(
    precedent_resolution_pathway,
    'Will a clean case on GPL linking scope reach definitive appellate resolution, and if so under which reading?',
    'Docket monitoring for a contested linking-scope case with standing satisfied and parties unwilling to settle; the SFC v. Vizio standing dismissal (2022) illustrates the current barrier.',
    'A definitive strong ruling converts this arrangement into the strong reading''s constraint (high epsilon, extensive victim set); a definitive narrow ruling dissolves the enforcement-vacuum arrangement entirely (this story''s constraint ceases to exist as the operative arrangement); continued judicial avoidance preserves the plurality and this story''s validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_resolution_pathway, empirical, 'Whether the kernel resolves judicially and in which direction.').

omega_variable(
    enforcement_capacity_trajectory,
    'Does FSF-aligned enforcement capacity recover (new standing theories, regulatory leverage such as supply-chain security rules) or continue its post-Vizio decay?',
    'Track enforcement actions brought and their outcomes, standing rulings, and new regulatory hooks over the next decade.',
    'Recovery tilts marginal contexts back toward community-set scope and raises the arrangement''s effective suppression; continued decay consolidates industry-side scope-setting and pushes the arrangement toward pure coordination with a shrinking extractive residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Direction of enforcement-capacity distribution across the two ecologies.').

omega_variable(
    ambiguity_coordination_vs_tax,
    'Is the interpretive plurality a coordination good — the only stable arrangement under which a single commons can serve two incompatible ecologies — or an extraction device that taxes certainty?',
    'Counterfactual analysis: model ecosystem composition under each sibling reading as settled law and compare total commons output and cost distribution against the plurality baseline.',
    'If coordination-good, most measured cost imposition is the price of the dual-ecology commons and the verdict sits near the rope boundary; if tax, the imposition is surplus and the verdict tilts toward the snare boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_coordination_vs_tax, conceptual, 'The rope/tangled-rope boundary question for this arrangement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (no forum sells certainty; enforcement threats deliberately left untested) or internalized (reputational fear and community norms driving over-compliance beyond measured legal risk)?',
    'Post-decision audits of adopter behavior: compare actual legal exposure in resolved disputes against pre-adoption risk assessments; survey compliance spending against measured risk.',
    'If substantially internalized, the arrangement''s effective suppression exceeds the structural measure and persists even if enforcement capacity collapses; clarity-seeker over-compliance is then a norm-internalization effect rather than a capacity effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in adopter compliance behavior.').

omega_variable(
    fsf_identity_fusion_persistence,
    'Is the FSF-aligned enforcement posture maintained because it remains functional, or because the organizations'' identities are fused with strong-copyleft advocacy — and would the posture survive an identity-frame break?',
    'Observe organizational behavior under continued capacity decay: whether resources shift toward clarifying litigation, standard-setting, or license revision rather than enforcement-as-usual.',
    'If identity-fused, enforcement theater persists past functional utility and the arrangement''s theater_ratio climbs further; if functional, capacity decay should produce visible strategic pivots.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsf_identity_fusion_persistence, empirical, 'Identity-lock dynamics in the community-enforcement seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_vacuum_tr_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t1991, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t1997, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1997, 0.22).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t1997, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2003, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2003, 0.35).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2003, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2009, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2009, 0.45).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2009, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2015, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2020, 0.5).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2020, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2025, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gpl_vacuum_be_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1991, 0.18).
narrative_ontology:measurement_basis(gpl_vacuum_be_t1991, observed).
narrative_ontology:measurement(gpl_vacuum_be_t1997, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1997, 0.26).
narrative_ontology:measurement_basis(gpl_vacuum_be_t1997, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2003, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2003, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2009, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2009, 0.46).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2009, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2015, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2020, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2025, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl_vacuum_su_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1991, 0.15).
narrative_ontology:measurement_basis(gpl_vacuum_su_t1991, observed).
narrative_ontology:measurement(gpl_vacuum_su_t1997, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1997, 0.22).
narrative_ontology:measurement_basis(gpl_vacuum_su_t1997, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2003, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2003, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2009, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2009, 0.48).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2009, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2015, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2020, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2025, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2025, 0.4).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'GPL copyleft scope' decomposes into three epsilon-invariant stories — the strong reading (high epsilon: extensive cost imposition on proprietary combiners), the narrow reading (near-zero epsilon: minimal binding force), and this enforcement-vacuum reading (moderate-low epsilon: the licensed plurality itself as the operative arrangement). Each story has its own beneficiaries, victims, and claimed type. They are linked here because the license text (the kernel) is upstream of all three, and each reading's practical force conditions the others': community enforcement capacity is what keeps the strong reading live in FSF-aligned contexts, and industry practice is what keeps the narrow reading operative everywhere else. This story's epsilon (0.38) is authored only for the enforcement-vacuum arrangement — the state in which no definitive precedent exists and operative scope is set by enforcement capacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
