% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Provincial Sovereignty Boundary — Compact Federalism Reading
 *   domain: political economy/federalism/resource governance
 *
 * SUMMARY:
 *   This story instantiates the compact_federalism reading of the
 *   provincial_sovereignty_boundary kernel: the boundary arrangement in which
 *   federal authority is conditional on provincial consent, provinces retain
 *   residual sovereignty as compact parties, equalization is negotiable among
 *   parties, climate policy is subject to provincial override, and exit runs
 *   through negotiation rather than permission. The ε referent is that
 *   compact-instantiated boundary as it actually operates — consent points
 *   administered by first ministers, litigated boundaries, renegotiated
 *   transfers — assessed by the reading's own lights, which prize consensual
 *   legitimacy and party equality. KEY AGENTS (by structural relationship):
 *   resource_rich_provinces — primary beneficiary (powerful/constrained),
 *   shields royalties and overrides federal climate measures while paying
 *   donor transfers; quebec_compact_party — beneficiary
 *   (organized/identity_locked), collects asymmetry recognition and transfer
 *   entitlements; have_not_recipient_provinces — primary target among
 *   provinces (moderate/trapped), carries negotiability risk;
 *   federal_government — dual-positioned payer-beneficiary
 *   (institutional/constrained), cedes contested-domain capacity while
 *   collecting the stability dividend; indigenous_nations — excluded target
 *   (organized/trapped), sovereignty asserted over them without their
 *   consent; interprovincial_trade_dependent_businesses — diffuse payer
 *   (moderate/constrained); supreme_court_canada — analytical observer;
 *   provincial_first_ministers — agenda_setters administering the consent
 *   points. The claim/metric gap is deliberate: the reading CLAIMS a
 *   consensual compact while the authored metrics describe moderately high,
 *   actively enforced, leverage-weighted operation — the engine measures the
 *   divergence per seat; nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - resource_rich_provinces: primary beneficiary (powerful/constrained) — collects royalty shielding and climate-override wins, pays donor transfers
 *   - quebec_compact_party: beneficiary (organized/identity_locked) — collects asymmetry recognition; identity fused with the compact frame
 *   - have_not_recipient_provinces: primary provincial target (moderate/trapped) — bears transfer-negotiability risk
 *   - federal_government: dual payer-beneficiary (institutional/constrained) — cedes contested-domain capacity, collects union-stability legitimacy
 *   - indigenous_nations: excluded target (organized/trapped) — provincial sovereignty asserted over unceded territories without consent
 *   - interprovincial_trade_dependent_businesses: diffuse payer (moderate/constrained) — absorbs thirteen-regime compliance costs
 *   - supreme_court_canada: analytical observer (institutional/analytical) — adjudicates and periodically repudiates the frame
 *   - provincial_first_ministers: agenda_setter (institutional/constrained) — administers consent points; individuals churn, class persists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.6).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.58).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty Boundary — Compact Federalism Reading").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political economy/federalism/resource governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '537930f8-c393-4d8a-a50d-abced46b6211').
narrative_ontology:cs_kernel_codification('537930f8-c393-4d8a-a50d-abced46b6211', fixed_text).
narrative_ontology:cs_authority_grounding('537930f8-c393-4d8a-a50d-abced46b6211', lineage).
narrative_ontology:cs_interpretation_layer_present('537930f8-c393-4d8a-a50d-abced46b6211').
narrative_ontology:cs_reading_relation('537930f8-c393-4d8a-a50d-abced46b6211', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('537930f8-c393-4d8a-a50d-abced46b6211', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('537930f8-c393-4d8a-a50d-abced46b6211', foundational, provinces_are_sovereign_compact_parties).
narrative_ontology:cs_axiom_status(provinces_are_sovereign_compact_parties, holdable).
narrative_ontology:cs_axiom_grounding('537930f8-c393-4d8a-a50d-abced46b6211', provinces_are_sovereign_compact_parties, deontological).
narrative_ontology:cs_axiom('537930f8-c393-4d8a-a50d-abced46b6211', secondary, secession_requires_negotiation_not_permission).
narrative_ontology:cs_axiom_status(secession_requires_negotiation_not_permission, holdable).
narrative_ontology:cs_axiom_grounding('537930f8-c393-4d8a-a50d-abced46b6211', secession_requires_negotiation_not_permission, conventional).
narrative_ontology:cs_reference_frame('537930f8-c393-4d8a-a50d-abced46b6211', confederation_compact_of_sovereign_provinces).
narrative_ontology:cs_drift_state('537930f8-c393-4d8a-a50d-abced46b6211', post_patriation_constitutional_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('537930f8-c393-4d8a-a50d-abced46b6211', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, quebec_compact_party).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, have_not_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, indigenous_nations).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, interprovincial_trade_dependent_businesses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hydrocarbon- and potash-endowed provincial governments (the Alberta-Saskatchewan pattern) own their natural resources under s.92A and collect royalties directly. They contribute to equalization as donor provinces while arguing the formula penalizes resource revenue. They litigate federal climate and project-review legislation as intrusion on exclusive provincial jurisdiction, and have passed sovereignty-style statutes asserting the right to refuse federal measures within their borders. Leaving the federation is not a live option; their leverage runs through courts, intergovernmental councils, and withheld cooperation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces, payer).

% Quebec's governments and much of its political class hold the founding-nation account: two peoples consented to a compact, and Quebec's distinct language, civil law, and social programs justify opt-outs, asymmetric recognition, and a veto claim over constitutional change. Quebec is the largest long-run equalization recipient while insisting the transfer is an entitlement owed a founding party rather than assistance. Its political identity is fused with the compact narrative itself — abandoning the frame would mean re-describing the province as simply one of ten — so the frame is maintained across otherwise rival parties.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, quebec_compact_party, beneficiary,
    organized, civilizational, identity_locked, national).

% Fiscal-dependent provincial governments (Maritime and prairie recipients) budget around equalization receipts that the compact frame renders perpetually renegotiable. Each renewal round exposes them to donor-province leverage and formula-change risk; they cannot replace the transfers with own-source revenue and cannot credibly threaten departure. They carry the uncertainty cost of the negotiability the compact reading insists on.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, have_not_recipient_provinces, payer,
    moderate, biographical, trapped, regional).

% Ottawa holds enumerated powers (trade, criminal law, taxation, emergency authority) and a spending power it uses to attach conditions to transfers. Under the compact frame its authority in contested domains — climate, resource project review, cultural policy — is treated as conditional on provincial acquiescence, so it purchases consent with money, exemptions, or forbearance. It also collects the frame's stability dividend: a federation understood as consensual is harder to break than one understood as imposed, which is why Ottawa alternately resists and invokes compact language.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government, beneficiary).

% First Nations, Inuit, and Métis governments were not parties to the 1867 bargain and were never asked to consent to it. Provincial sovereignty is asserted daily over territories where Aboriginal title remains unresolved; court-recognized consultation duties give procedural voice but not the consent power the compact logic reserves for signatories. They hold growing litigation capacity and UNDRIP-based statutory footholds, but no seat at the tables where the boundary is actually renegotiated.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, excluded,
    organized, generational, trapped, national).

% Firms moving goods, services, credentials, and alcohol across provincial lines absorb the compliance cost of thirteen regulatory regimes that provincial jurisdictional autonomy sustains. They lobby for mutual recognition and occasionally win narrow agreements, but cannot vote any single government out of the barrier-maintenance equilibrium and cannot relocate away from the domestic market they depend on.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, interprovincial_trade_dependent_businesses, payer,
    moderate, biographical, constrained, national).

% The Court adjudicates boundary disputes and has shaped the frame's limits: it rejected the pure compact theory in the Secession Reference (holding the Constitution constitutes a new sovereign order rather than a treaty among sovereign states) while conceding a negotiated-exit duty, and it split over federal carbon-pricing authority before upholding the backstop. It neither collects nor pays; its pronouncements redefine what each seat can claim.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, supreme_court_canada, observer,
    institutional, generational, analytical, national).

% Premiers collectively administer the consent points the compact frame creates: constitutional amendment ratification, intergovernmental councils, transfer negotiations. Individually each premier's tenure is short and electoral, so each spends accumulated leverage quickly; the class persists while the individuals churn. They set the agenda for which boundary disputes reach crisis.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_first_ministers, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides governing authority across a continental, regionally heterogeneous polity so that local majorities govern local matters; protects linguistic and religious minorities through jurisdictional guarantees; and addresses the commitment problem that keeps large regions inside a union they could dominate or be dominated by — each order of government can rely on the boundary because both sides ratified it.
% TRANSFER_FUNCTION: Moves fiscal transfers (equalization, health and social transfers) from richer to poorer provinces on terms renewed through negotiation; moves governing capacity in contested domains (climate, project review) from Ottawa to whichever order holds leverage; moves bargaining concessions (opt-outs, exemptions, moratoria) from the federal government to leveraged provinces; retains resource royalty streams in owner provinces.
% ABSENT_VOICES: Indigenous nations (never signatories; consent never sought), municipal governments (creatures of the provinces, absent from first ministers' tables), future generations (costs of provincially overridden climate policy are deferred to them), and interprovincial migrants and businesses bearing barrier costs without a seat in any negotiation.
% DISAPPEARANCE_RATIONALE: If the compact-instantiated boundary vanished overnight, the federation would reorganize around one of its rivals: either consolidated federal supremacy (uniform climate regulation, formulaic non-negotiable transfers, unitary project review) or intensified provincial fragmentation (resource separatism, bilateral deals, reopened secession questions). Equalization would collapse or entrench, thirteen regulatory regimes would merge or multiply, and every seat's bargaining position would reset.
% FOUNDING_PROBLEM: How to unite autonomous colonies into a viable continental union without any party surrendering entirely — protecting Catholic francophone Lower Canada and the smaller Maritime colonies from majority assimilation while building a common economy and defense. The compact framing answered: a union of consenting parties, not a conquest.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Supreme Court's Secession Reference (1998) attests the founding problem — reconciling union with regional and minority protection — while explicitly rejecting the compact-among-sovereign-provinces characterization; constitutional historians across both centralist and provincial-rights traditions attest the 1867 bargain's minority-protection motives; Indigenous oral histories and modern title litigation attest that no compact ever included the peoples over whom sovereignty was asserted. No corroborating source outside the frame's adherents treats the compact characterization itself as settled.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.60 at interval end) because the boundary's operation channels bargaining rents to whoever holds leverage: resource royalties stay sheltered, contested-domain capacity migrates to the leveraged order, and transfer terms renew under donor pressure. Suppression (0.58) reflects enforcement by institutional means — litigation, withheld consent, spending conditionality, sovereignty statutes — rather than raw force; alternatives are pressured, not erased. Theater (0.42) has risen steadily as symbolic sovereignty statutes, equalization referenda, and communiqué diplomacy multiply while binding outcomes thin; the ratio stays below half because real transfers flow and real vetoes bind. Accessibility collapse is moderate (0.48): rival framings — centralized, asymmetric, cooperative, dualist — remain live, which is precisely why three sibling readings of the kernel coexist. Resistance is high (0.66): the Supreme Court repudiated pure compact theory in 1998, federal governments assert supremacy episodically, Indigenous nations contest the frame's exclusions, and centralist provinces push back. The temporal series run on one shared seven-point grid (1867, 1892, 1937, 1982, 1998, 2018, 2026) with all three metrics authored at every point. The extractiveness trajectory is monotonic accumulation rather than cycle: each settlement institutionalizes new leverage infrastructure (Judicial Committee provincial-rights dicta, the 1982 amendment formula and s.92A, the 1998 negotiated-exit duty, the carbon-backstop era), layering rent-bearing machinery onto the coordination core. The 1982 suppression spike marks the patriation breach and the formalization of consent enforcement; the 1998 dip marks the post-referendum settlement before enforcement intensity rebuilt.
 *
 * PERSPECTIVAL GAP:
 *   Four seats should compute materially different types from identical structural data. From the resource_rich_provinces seat the boundary operates as protective coordination — jurisdictional shelter against federal overreach, worth its donor contributions. From the have_not_recipient_provinces seat the same negotiability operates as exposure — annual leverage over budgets they cannot escape, a harsher computed profile. From the indigenous_nations seat the frame is assertion without consent — the reading's own consensual principle condemns the arrangement that excludes them. From the federal_government seat the picture is genuinely mixed, which is why no directionality override is authored: the derivation reads the declared positions, and the dual position is left to per-seat computation. Coalition potential matters for the trapped seats: have-not provinces, Ottawa, and Indigenous nations form a latent counter-coalition against donor leverage, but their horizons and scopes diverge enough that it has rarely crystallized. Quebec's identity lock deserves separate note: the fusion is relational and ideological — a founding-nation self-concept constituted through the compact narrative, maintained across rival parties — so its computed extraction stays damped even in rounds where its material position worsens; if the identity frame broke, the seat would recompute as an ordinary province and the asymmetry gains would become visible as rents.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for resource_rich_provinces (net collectors even after donor payments — royalty shielding dwarfs contributions) and quebec_compact_party (recipients of asymmetry recognition and transfer entitlements, with identity-lock damping perceived cost). Victim declarations drive high directionality for have_not_recipient_provinces (transfer-negotiability risk with no exit), indigenous_nations (sovereignty asserted over them without consent — structurally the fullest targets in the story despite holding an excluded seat at the table), and interprovincial_trade_dependent_businesses (diffuse barrier costs, constrained relocation). The federal government sits mid-range: it cedes contested-domain capacity (target-side) while collecting the consensual-legitimacy dividend (beneficiary-side); it is deliberately left to structural derivation rather than overridden, and the commentary flags the dual position rather than forcing a single d. Scope amplification applies modestly: the boundary operates at national scale, where verification of consent and compliance is harder and effective extraction scales up accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — minority protection plus union viability — has partially migrated rather than died: language and education protections moved into the Charter era (section 23, the notwithstanding clause), while the compact frame persists chiefly as bargaining infrastructure for leverage. Because the status is contested rather than dead, the mismatch consumer reads contested x world_rearranges and no zombie flag fires; mandatrophy is accordingly NOT declared resolved. The tangled_rope classification earns its keep by blocking both symmetrical errors: reading the boundary as pure rope would hide the leverage-weighted extraction (donor dictates, overridden climate policy, excluded nations), while reading it as a snare would erase the real coordination delivered — regional self-government, minority jurisdictional guarantees, and a consensual union that has held together a continental polity for 159 years. It is not a piton: theater is rising but below half, real transfers flow, real vetoes bind, and the function is alive even where its 1867-specific terms are obsolete. Fixing cost is prohibitive for every seat — the 7/50 amendment formula that the frame itself administers makes unilateral repair impossible, which is why degradation accumulates rather than resolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confederation_compact_historicity,
    'Was Confederation in fact a compact among sovereign provinces, or a legislative union constituting a new sovereign order?',
    'Archival and doctrinal synthesis: the Secession Reference already weighed the question against the compact theory; continued constitutional-historical scholarship and any future reference decision could consolidate or reopen it.',
    'If the legislative-union account prevails definitively, the compact reading''s veto and exit claims lose their foundation and this constraint''s shape converges toward the constitutional_subordination sibling''s victim structure; if the compact account strengthens, provincial consent points harden and extraction migrates further toward federal capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confederation_compact_historicity, conceptual, 'Historical-conceptual ambiguity in the founding character of the federation.').

omega_variable(
    indigenous_consent_gap,
    'Can a consent-grounded compact confer legitimacy when the peoples over whom sovereignty was asserted never consented to it?',
    'UNDRIP implementation trajectory, Tsilhqot''in-line title litigation outcomes, and whether free-prior-informed-consent standards come to apply to provincial assertions of jurisdiction.',
    'Applying the reading''s own consent principle universally converts indigenous_nations from an excluded seat into a fully counted target, raising effective extraction sharply and destabilizing the frame''s legitimacy claim; declining to apply it exposes the consent principle as selective, corroding the coordination half of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_consent_gap, conceptual, 'Whether the compact''s consent logic is universalizable or self-exempting.').

omega_variable(
    equalization_negotiability_effect,
    'Does making equalization negotiable express compact equality among parties, or entrench donor leverage over recipients?',
    'Longitudinal fiscal data: transfer stability across renewal rounds, recipient borrowing costs, and documented donor-province demands during negotiations.',
    'If leverage dominates, the have_not seat''s computed extraction rises toward the harsher end and the coordination half of the tangled rope thins; if negotiation tracks genuine fiscal-capacity updating, the equality framing holds and the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equalization_negotiability_effect, empirical, 'Whether transfer negotiability functions as parity or as leverage.').

omega_variable(
    exit_option_reality,
    'Is negotiated exit a live option structure, or permission-gated in practice through clear-question and clarity conditions and federal gatekeeping?',
    'Comparative secession-law analysis and revealed behavior across referendum episodes: whether a clear majority on a clear question has reliably triggered negotiation, and what conditions were attached.',
    'If exit is effectively gated, suppression is understated and trapped seats compute harsher profiles, pulling the constraint toward snare flavor for those seats; if genuinely negotiable, the frame''s consensual legitimacy claim strengthens and the rope component firms up.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_option_reality, empirical, 'Reality-status of the negotiated-exit option the reading advertises.').

omega_variable(
    sibling_reading_delta_routing,
    'This constraint is the compact_federalism reading of kernel provincial_sovereignty_boundary — what would each sibling reading change structurally, and where is the disagreement located?',
    'Compile and compare the sibling stories: constitutional_subordination relocates the victim set to provinces-under-Ottawa generally; resource_sovereignty_primacy concentrates extraction on federal climate capacity and Indigenous title holders. The disagreement is located in the SOURCE of provincial authority: compact consent versus constitutional grant versus resource ownership.',
    'Epsilon and victim sets are reading-indexed over a shared kernel; any cross-reading comparison must route through the decomposed family, not through averaging or re-measuring this file under a sibling''s observable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta_routing, conceptual, 'Committer-frame routing: sibling deltas and the locus of the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1867, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1867, 0.15).
narrative_ontology:measurement_basis(prov_tr_t1867, observed).
narrative_ontology:measurement(prov_tr_t1892, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1892, 0.18).
narrative_ontology:measurement_basis(prov_tr_t1892, observed).
narrative_ontology:measurement(prov_tr_t1937, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1937, 0.22).
narrative_ontology:measurement_basis(prov_tr_t1937, observed).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.3).
narrative_ontology:measurement_basis(prov_tr_t1982, observed).
narrative_ontology:measurement(prov_tr_t1998, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1998, 0.33).
narrative_ontology:measurement_basis(prov_tr_t1998, observed).
narrative_ontology:measurement(prov_tr_t2018, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2018, 0.38).
narrative_ontology:measurement_basis(prov_tr_t2018, observed).
narrative_ontology:measurement(prov_tr_t2026, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(prov_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1867, 0.34).
narrative_ontology:measurement_basis(prov_be_t1867, observed).
narrative_ontology:measurement(prov_be_t1892, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1892, 0.37).
narrative_ontology:measurement_basis(prov_be_t1892, observed).
narrative_ontology:measurement(prov_be_t1937, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1937, 0.41).
narrative_ontology:measurement_basis(prov_be_t1937, observed).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.49).
narrative_ontology:measurement_basis(prov_be_t1982, observed).
narrative_ontology:measurement(prov_be_t1998, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1998, 0.53).
narrative_ontology:measurement_basis(prov_be_t1998, observed).
narrative_ontology:measurement(prov_be_t2018, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2018, 0.57).
narrative_ontology:measurement_basis(prov_be_t2018, observed).
narrative_ontology:measurement(prov_be_t2026, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2026, 0.6).
narrative_ontology:measurement_basis(prov_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1867, 0.25).
narrative_ontology:measurement_basis(prov_su_t1867, observed).
narrative_ontology:measurement(prov_su_t1892, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1892, 0.27).
narrative_ontology:measurement_basis(prov_su_t1892, observed).
narrative_ontology:measurement(prov_su_t1937, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1937, 0.34).
narrative_ontology:measurement_basis(prov_su_t1937, observed).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.52).
narrative_ontology:measurement_basis(prov_su_t1982, observed).
narrative_ontology:measurement(prov_su_t1998, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1998, 0.48).
narrative_ontology:measurement_basis(prov_su_t1998, observed).
narrative_ontology:measurement(prov_su_t2018, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement_basis(prov_su_t2018, observed).
narrative_ontology:measurement(prov_su_t2026, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(prov_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, resource_allocation).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, equalization_program_entrenchment).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, federal_climate_backstop_authority).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'provincial sovereignty' covers three structurally distinct claims that share one kernel (the provincial sovereignty boundary) and diverge on the source of provincial authority. This story instantiates the compact_federalism reading (consent-derived residual sovereignty, negotiable transfers, negotiated exit). The constitutional_subordination sibling authors a different constraint over the same territory (granted jurisdiction, no inherent sovereignty, permission-gated exit) with provinces generally in the target set; the resource_sovereignty_primacy sibling authors a third (ownership-derived absolute sovereignty) concentrating extraction on federal climate capacity and Indigenous title. Upstream/downstream structure: the compact reading's consent logic legitimizes and pressures the resource-sovereignty sibling without foreclosing it; the subordination sibling competes as a live coexisting position held by centralist parties and articulated in Secession Reference dicta. Each member carries its own epsilon, beneficiaries, and victims; the family is linked exclusively through network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
