% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Constitutional Subordination of Provinces: Federal Consent Gate on Sovereignty and Exit
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   The constraint under story is the constitutional-subordination settlement
 *   of Canadian federalism as it stands: provincial governments exercise
 *   powers delegated under a supreme written constitution (Constitution Acts
 *   1867-1982), possess no inherent or residual sovereignty, and can alter
 *   their membership only through an amendment process the federal side
 *   effectively gates (the 7/50 general formula; the Clarity Act's federal
 *   clarity determination preconditioning any secession negotiation). The
 *   settlement delivers real continental coordination - defense, currency,
 *   internal free movement, pooled commodity-shock insurance - while bearing
 *   asymmetrically on provinces whose policy centers diverge from Ottawa's:
 *   resource-exporting donor provinces finance the transfer system and submit
 *   provincially owned resources to federal assessment and emissions regimes
 *   they contest, and autonomist Quebec carries a legally blocked
 *   self-determination claim. This story is ONE reading of the contested
 *   kernel provincial_sovereignty_boundary; the sibling readings
 *   (compact_federalism, resource_sovereignty_primacy) are separate
 *   constraints with their own epsilon, victim sets, and classifications,
 *   linked via network.affects_constraints. Epsilon's referent is the
 *   standing subordination arrangement as this reading assesses it - not the
 *   compact arrangement the sibling reading would install. Claim and metrics
 *   are independent facts: the claimed type records the structure I judge
 *   true (a genuine coordination function joined to asymmetric extraction
 *   under active enforcement); the metrics record the operation I observe,
 *   including its drift.
 *
 * KEY AGENTS:
 *   - federal_parliament: agenda-setter and principal beneficiary (institutional/arbitrage) - holds reserved exit-decision rights, sets transfer conditionality, extends federal regulation over provincial-resource jurisdiction
 *   - supreme_court_of_canada: enforcement adjudicator holding an analytical seat with a beneficiary edge (institutional/constrained) - accrues interpretive jurisdiction from the supremacy framework it enforces
 *   - resource_donor_provinces: primary payer (institutional/constrained) - net fiscal contributors facing federal regimes over provincially owned resources; exit legally gated
 *   - quebec_nationalists: payer with identity-fused exit (organized/identity_locked) - blocked self-determination claim carried by a fused national identity
 *   - recipient_provinces: coordinated beneficiaries (institutional/constrained) - fiscal position presupposes the equalization architecture
 *   - interprovincial_mobile_citizens: diffuse coordinated public (organized/mobile) - consume federation-wide goods, fund the transfers
 *   - indigenous_nations: excluded seat (organized/trapped) - hold pre-1867 claims to the sovereignty being allocated; absent from the amending formula
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.7).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.62).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.7).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination of Provinces: Federal Consent Gate on Sovereignty and Exit").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'a9cbc8b4-f30d-4cde-9424-5d417eed3ab9').
narrative_ontology:cs_kernel_codification('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', fixed_text).
narrative_ontology:cs_authority_grounding('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', lineage).
narrative_ontology:cs_interpretation_layer_present('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9').
narrative_ontology:cs_reading_relation('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', provincial_sovereignty_boundary__resource_sovereignty_primacy, forecloses).
narrative_ontology:cs_axiom('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', foundational, imperial_enactment_not_compact).
narrative_ontology:cs_axiom_status(imperial_enactment_not_compact, holdable).
narrative_ontology:cs_axiom_grounding('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', imperial_enactment_not_compact, conventional).
narrative_ontology:cs_axiom('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', foundational, constitution_supremacy_over_provincial_authority).
narrative_ontology:cs_axiom_status(constitution_supremacy_over_provincial_authority, holdable).
narrative_ontology:cs_axiom_grounding('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', constitution_supremacy_over_provincial_authority, conventional).
narrative_ontology:cs_axiom('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', secondary, secession_requires_amendment_formula).
narrative_ontology:cs_axiom_status(secession_requires_amendment_formula, holdable).
narrative_ontology:cs_axiom_grounding('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', secession_requires_amendment_formula, conventional).
narrative_ontology:cs_reference_frame('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', centralized_union_settlement_1867).
narrative_ontology:cs_drift_state('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', contemporary_resource_conflict_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a9cbc8b4-f30d-4cde-9424-5d417eed3ab9', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_parliament).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, recipient_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, interprovincial_mobile_citizens).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_donor_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, quebec_nationalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds reserved decision rights over provincial exit: no province alters its membership or boundaries except through an amendment process requiring federal participation, and the Clarity Act conditions any secession negotiation on a clarity determination the federal House makes for itself. Sets conditions attached to major transfers, extends federal regulation into fields touching provincial jurisdiction through the national-concern branch of the peace, order and good government power, and collects the compliance of ten provincial governments without needing their agreement on exit questions. Its position in the arrangement is the position that wrote the arrangement.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicates the division of powers and the meaning of the constitution: upheld the federal greenhouse-gas pricing scheme under the national-concern doctrine in 2021, and struck down parts of the federal Impact Assessment Act in 2023 as overreach into provincial jurisdiction. Its interpretive authority, including the living-tree method that lets the constitution move without formal amendment, exists only inside a framework in which the written constitution is the single supreme source of governmental legitimacy. It decides the contests; it does not set the agenda, and it is bound by its own precedents.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, beneficiary).

% Alberta and Saskatchewan (Newfoundland in earlier decades) pay substantially more into federal coffers than they receive back, and hold constitutional ownership of their natural resources under s. 92A. They face federal environmental assessment and emissions regimes applied to those provincially owned resources over their recorded objection. Their remedies all operate inside the consent architecture they contest: litigation (partially successful in 2023), a provincial sovereignty-within-a-united-Canada statute that declares but does not displace federal law, and a symbolic referendum on deleting the constitutional entrenchment of equalization. Leaving the federation would mean abandoning the national market their exports sell into, and is legally unavailable without federal consent.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_donor_provinces, payer,
    institutional, biographical, constrained, national).

% Carry a self-determination claim expressed in two province-wide votes (1980, 1995 - the second failing by half a percentage point). After 1995 the legal answer hardened: the Supreme Court's 1998 reference held unilateral secession unconstitutional under both domestic and international law, and the Clarity Act made any negotiation contingent on a federal clarity judgment. Their political identity is fused with the nationhood project, so continued membership in a federation that formally denies their collective status is experienced as a standing cost; yet the only exit door opens onto a corridor whose locks the federal side controls. Symbolic concessions (the 2006 nation motion) arrive without transferable authority.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, quebec_nationalists, payer,
    organized, generational, identity_locked, national).

% A rotating majority of provinces (Quebec largest among them) receive equalization payments that let them deliver comparable public services at comparable tax rates without comparable own-source revenue. Their fiscal position presupposes the transfer architecture and the federal taxing room that funds it; exiting the arrangement would mean forfeiting receipts they cannot replace. They defend federal spending power in principle and resist conditionality in particular cases.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, recipient_provinces, beneficiary,
    institutional, biographical, constrained, national).

% Move, work, bank, retire and claim pensions across ten provinces without border friction, under one currency, one external tariff, one defense perimeter, and portable credentials. None of the provincial units taken alone could guarantee these goods. They also pay the federal taxes that fund the transfer system, and encounter the sovereignty question mainly as background stability rather than as a daily constraint.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, interprovincial_mobile_citizens, beneficiary,
    organized, biographical, mobile, national).

% Hold treaty relationships that predate 1867 and claims of unceded territory that neither order of government consulted when allocating sovereignty between them. Section 35 channels their rights inside the constitutional order, but they hold no seat in the amending formula's arithmetic and were not parties to the settlements this reading ratifies. They would object that both the federal claim and the provincial claims proceed on terms their nations set differently; their objection is heard in specific-rights litigation but is structurally muted in sovereignty conversations.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_parliament).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A continental federation supplies goods no province can supply alone: external defense, a single currency and banking system, an internal market with guaranteed labor and capital mobility, macroeconomic stabilization, and fiscal insurance that pools commodity-shock risk across regions. The subordination settlement is the enforcement skeleton for those goods.
% TRANSFER_FUNCTION: Moves fiscal capacity annually from higher-income and resource-exporting provinces to lower-income provinces through the equalization program; moves policy discretion from provincial capitals to Ottawa wherever federal spending carries conditions or federal regulation reaches provincially owned resources; and reserves the decision on provincial exit to the federal-side amendment and clarity process.
% ABSENT_VOICES: Indigenous nations with pre-1867 treaty and unceded-territory claims would object that the sovereignty being allocated was theirs to concede, and they are absent from the amending formula that governs the arrangement. Donor-province electorates opposed to the transfer scale are present only through their provincial governments, which hold no vote in the general amendment formula proportional to their contribution. Both objections are registered in litigation and protest rather than in the decision procedure itself.
% DISAPPEARANCE_RATIONALE: If the subordination settlement vanished overnight, ten provincial governments would hold undefined residual authority, the currency and defense perimeter would need renegotiating among previously subordinate units, the equalization pool would evaporate along with its financing, and any province could relitigate its membership at will - the entire architecture of Canadian public finance, external representation, and internal mobility would have to be rebuilt from scratch.
% FOUNDING_PROBLEM: Unite fragmented British North American colonies into a single polity capable of continental defense and economics while preserving local legislatures, and do so while avoiding the states-rights structure that had just produced the American Civil War. The 1867 design answered by creating provinces subordinate to a central sovereign authority rather than a compact of sovereign states.
% FOUNDING_PROBLEM_CORROBORATION: The union problem itself is corroborated from outside the beneficiary set: constitutional historians relying on the Quebec Resolutions and Confederation-era debates document the deliberate rejection of the American model, and sovereigntist and western-autonomy leaders - the arrangement's principal opponents - concede the reality of the shared-market and defense rationale even as they reject the subordination form. What no outside party corroborates is the claim that subordination remains the uniquely viable form; that is contested by the same opposing seats.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.70 because the arrangement's burdens concentrate on identifiable seats: the exit veto is priced only by those who want to use it (Quebec nationalists, and increasingly donor provinces), and federal climate and assessment authority reaches resources the constitution assigns to provincial ownership. Suppression is 0.62 - high but not total: provinces retain wide ordinary autonomy inside their sphere, so the suppressive force is concentrated on exactly two vectors (exit and jurisdictional retreat) rather than diffuse. Theater is 0.28: the machinery mostly functions, but a growing share of activity is consultative ritual - pre-consultation tours, symbolic recognition motions, declaratory provincial sovereignty statutes that change no legal position - which is why theater_ratio trends upward in the series without approaching piton territory. Accessibility_collapse is 0.58: once the architecture is understood, unilateral exit and extra-constitutional assertion visibly fail, but political routes (referendum pressure, negotiated amendment) remain nominally open, so alternatives dim rather than vanish. Resistance is 0.60 and sustained: two sovereignty referenda, division-of-powers litigation with a partial win in 2023, equalization referendum campaigns, and declaratory autonomy statutes. The suppression_requirement series is authored deliberately because this story traces enforcement-capacity build-up: the Clarity Act (2000) hardened the exit gate, the 2021 carbon-pricing reference confirmed federal reach under the national-concern doctrine, and conditionality machinery matured across the interval - a rising enforcement trajectory, not a static picture, so the scalar base_properties.suppression alone would understate it. All three tracked series share one time grid (points 0, 5, 10, 15, 20, 25, 30, mapped to approximately 1995-2025, one unit per year) with every metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical nominal standing. All provincial governments are institutional actors, yet recipient provinces compute a subsidized seat (their fiscal position depends on the arrangement continuing) while donor provinces compute a heavily burdened seat (they finance the pool and absorb the jurisdictional intrusions) - the divergence is constraint-specific (transfer direction plus resource jurisdiction), not a difference in global power. The Quebec seat diverges further through identity lock: because the nationhood project is fused with political identity, the blocked exit is experienced as a standing injury regardless of material flows, so its computed burden exceeds what fiscal data alone would predict. The federal seat computes coordination it built and defends; the Court seat computes a constitutive framework it administers; the citizen seat computes background stability. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal parliament sits nearest the beneficiary end (authors the rules, collects the compliance, holds the reserved veto rent). Recipient provinces are beneficiaries with constrained exit - subsidized but bound. Interprovincial-mobile citizens are broadly net beneficiaries of the union goods, though they also fund the transfers, so their true position sits nearer symmetric than a pure beneficiary declaration implies; the derivation from the beneficiary listing will read them somewhat more subsidized than they are, which commentary flags rather than patches with an override keyed to a shared power atom (an override at their power level would distort the Quebec and indigenous seats that share it). Resource-donor provinces and Quebec nationalists are targets: trapped or identity-locked, national scope, bearing both the fiscal transfer and the jurisdictional impositions. Receipt is distinguished from benefit: equalization dollars flow to recipient provinces, but the arrangement's extraction rents - the reserved veto, the conditionality leverage, the regulatory precedence over provincial resources - demonstrably accrue to the federal seat, which is why gain_flow names federal_parliament and not the transfer's destination. Indigenous nations are authored as excluded rather than victims: within this reading's own lights their claims are accommodated inside s. 35 rather than extracted from, but they are structurally absent from the conversation that allocates sovereignty - the omega on s. 35 documents that this placement is itself contestable.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as pure extraction would erase the coordination function no province can replicate - currency, defense, internal mobility, shock-pooling are real and continuously consumed. Reading it as pure coordination would erase the asymmetry: the exit veto's costs fall on identifiable dissenting provinces rather than diffusely, and the same instrument that guarantees the union prices the ambitions of specific members. The founding problem (continental union) remains live, so the founding_problem_status x disappearance_verdict cell reads live x world_rearranges - no zombie flag expected, and mandatrophy_resolved is not declared. The identity-lock dynamics matter for forward classification: if the Quebec identity frame continues to erode (support trends suggest it might), the nationalist seat's burden declines and the arrangement's extraction profile narrows toward the resource-donor axis alone - at which point the computed type migrates toward the coordination pole at the margin, driven by the structural data rather than by any retuning of the claim. Coalition dynamics cap the payer side's resistance: donor-province coalitions have been unstable (Newfoundland-Labrador's graduation from recipient status removed the sharpest shared-interest partner), which is why class-level resistance stays moderate despite intense single-province grievance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_delta,
    'This constraint instantiates only the constitutional_subordination reading of the provincial_sovereignty_boundary kernel - what would the sibling readings (compact_federalism, resource_sovereignty_primacy) change structurally if instantiated instead?',
    'Compile the sibling files and diff beneficiary/victim sets, directionality profiles, and enforcement requirements against this one. The disagreement locates at a single structural element: the SOURCE of provincial authority - constitutional delegation (this file) versus founding-compact residue (compact_federalism) versus territorial-resource ownership (resource_sovereignty_primacy).',
    'Under compact_federalism the federal exit veto converts from supremacy enforcement into a standing breach of compact obligation, making exit negotiable under duress and moving the victim set to include any province the center refuses to release. Under resource_sovereignty_primacy federal climate and assessment authority over provincially owned resources becomes illegitimate seizure, concentrating extraction almost entirely on the federal seat and dissolving the coordination-function gate for resource jurisdiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    amendment_rigidity_stability_tradeoff,
    'Does the practical impossibility of formal constitutional amendment (Meech Lake 1990, Charlottetown 1992) stabilize the federation by removing rupture surfaces, or accumulate rupture pressure by blocking peaceful adjustment?',
    'Comparative federal analysis correlating amendment-formula rigidity with secession-referendum recurrence, unilateral-action attempts, and extra-constitutional assertion (declaratory sovereignty statutes) across federations.',
    'If rigidity accumulates rupture, the measured suppression is purchasing future discontinuity and payer-seat classifications harden toward the extractive pole; if it stabilizes, the same suppression reads as the coordination overhead a continental union genuinely requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_rigidity_stability_tradeoff, empirical, 'Whether the consent gate''s rigidity is stabilizing or rupture-accumulating.').

omega_variable(
    clarity_act_neutral_gate_or_veto,
    'Does the Clarity Act''s clarity determination function as a neutral procedural gate that any clear majority could satisfy, or as a discretionary federal veto operative even after a clear referendum result?',
    'Examine the parliamentary record of the Act''s drafting alongside negotiation conduct in referendum-adjacent episodes; the test is whether a hypothetical overwhelming majority would trigger unconditional negotiation or further federal discretion.',
    'The discretionary reading raises the suppression actually borne by the secessionist seat above the structural measure and supports treating the exit gate as the arrangement''s principal extraction instrument; the neutral reading supports the coordination framing of the gate as rule-of-law sequencing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clarity_act_neutral_gate_or_veto, empirical, 'Status of the clarity requirement: procedure or veto.').

omega_variable(
    section35_protection_or_subordination,
    'Does s. 35''s channeling of indigenous treaty rights into the constitutional order protect those rights, or subordinate indigenous sovereignty to a framework those nations never acceded to as founding polities?',
    'Modern-treaty implementation outcomes, duty-to-consult jurisprudence effectiveness, and the operational effect of UNDRIP-aligned implementing legislation on the actual disposition of land and resource decisions.',
    'The protection reading keeps indigenous nations outside this constraint''s victim set (their exclusion is a defect of the broader constitutional conversation, not of this arrangement''s operation). The subordination reading adds a third victim class - nations whose sovereignty claims this reading''s own axiom nullifies - and raises the arrangement''s measured extraction accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section35_protection_or_subordination, conceptual, 'Boundary of the victim set with respect to indigenous sovereignty claims absorbed by s. 35.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0, 0.16).
narrative_ontology:measurement(prov_tr_t5, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 5, 0.17).
narrative_ontology:measurement(prov_tr_t10, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 10, 0.19).
narrative_ontology:measurement(prov_tr_t15, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 15, 0.21).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 20, 0.24).
narrative_ontology:measurement(prov_tr_t25, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 25, 0.26).
narrative_ontology:measurement(prov_tr_t30, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(prov_be_t5, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(prov_be_t10, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(prov_be_t15, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(prov_be_t25, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(prov_be_t30, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(prov_su_t5, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(prov_su_t10, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(prov_su_t15, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(prov_su_t20, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(prov_su_t25, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 25, 0.59).
narrative_ontology:measurement(prov_su_t30, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% The colloquial label 'who is sovereign in Canada' conflates three structurally distinct claims and is decomposed into a three-story constraint family per the epsilon-invariance principle. This file (constitutional_subordination) authors the legally dominant reading: authority derives from the constitution, exit runs through federal consent - epsilon approximately 0.70 over the standing arrangement, with a broad mixed victim set. provincial_sovereignty_boundary__compact_federalism authors the compact claim (provinces as former sovereigns with residual sovereignty, exit negotiable under duress) - a different constraint with a different victim set (any province the center refuses to release) and a different enforcement profile (moral-political rather than juridical). provincial_sovereignty_boundary__resource_sovereignty_primacy authors the ownership claim (s. 92A resource ownership as ground of absolute provincial sovereignty overriding federal climate authority) - concentrated epsilon on the federal-regulatory interface with provincially owned resources. This reading is upstream of both siblings in legitimacy terms: courts cite its supremacy logic against compact and ownership claims, so its operation shapes the operating environment (and the available exits) of the other two. Each file links the others via network.affects_constraints; the family shares the kernel_id provincial_sovereignty_boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
