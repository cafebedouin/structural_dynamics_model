% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Patent Exclusivity Interpretation (Pharmaceutical Innovation Focus)
 *   domain: international_trade/intellectual_property/public_health
 *
 * SUMMARY:
 *   This constraint instantiates the STRONG EXCLUSIVITY READING of the TRIPS
 *   agreement kernel. The reading interprets TRIPS text as mandating high,
 *   uniform patent protections with narrow, limited flexibilities for public
 *   health exceptions. It emphasizes that patent term (20 years) is
 *   universal, that compulsory licensing is restricted to narrow emergencies
 *   (national security, public health crises, anti-competitive practices),
 *   and that parallel imports can be restricted by patent holders. This
 *   reading is held by multinational pharmaceutical corporations,
 *   developed-country governments, and the WTO dispute settlement mechanism,
 *   which has consistently ruled in favor of strict patent enforcement and
 *   narrow exceptions. The competing PUBLIC_HEALTH_FLEXIBILITY_READING
 *   (separate constraint story) emphasizes TRIPS text's actual language on
 *   compulsory licensing (Article 31) and public health carve-outs (DOHA
 *   Declaration), arguing these are broad flexibilities, not narrow
 *   exceptions. The two readings contest the same TRIPS text but arrive at
 *   opposite beneficiary/victim structures: strong exclusivity makes patent
 *   holders beneficiaries and patients/developing countries victims;
 *   flexibility reading reverses this by centering access rights.
 *
 * KEY AGENTS:
 *   - multinational pharmaceutical corporations (institutional beneficiary/agenda_setter) — enforce patent exclusivity globally, capture pricing rents
 *   - developed-country governments (institutional beneficiary/agenda_setter) — host pharma sectors, set WTO negotiating positions
 *   - WTO dispute settlement mechanism (institutional agenda_setter) — interprets TRIPS to enforce strong exclusivity reading through binding rulings
 *   - low-income patients (powerless payer) — face patent-protected pricing, trapped in access deprivation
 *   - developing-country governments (moderate payer) — constrained from using compulsory licensing without trade retaliation risk
 *   - generic manufacturers (organized payer) — legally restricted from producing patented drugs even in public health crises
 *   - public health advocates and NGOs (excluded) — excluded from formal interpretation and dispute settlement authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.81).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Patent Exclusivity Interpretation (Pharmaceutical Innovation Focus)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade/intellectual_property/public_health").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f').
narrative_ontology:cs_kernel_codification('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', fixed_text).
narrative_ontology:cs_authority_grounding('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', extraction).
narrative_ontology:cs_interpretation_layer_present('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f').
narrative_ontology:cs_reading_relation('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', trips_agreement_interpretive_kernel__trips_public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_axiom('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', foundational, patent_protection_drives_pharmaceutical_innovation).
narrative_ontology:cs_axiom_status(patent_protection_drives_pharmaceutical_innovation, holdable).
narrative_ontology:cs_axiom_grounding('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', patent_protection_drives_pharmaceutical_innovation, empirically_contingent).
narrative_ontology:cs_axiom('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', foundational, uniform_global_patent_standards_reduce_arbitrage_rent_dissipation).
narrative_ontology:cs_axiom_status(uniform_global_patent_standards_reduce_arbitrage_rent_dissipation, holdable).
narrative_ontology:cs_axiom_grounding('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', uniform_global_patent_standards_reduce_arbitrage_rent_dissipation, empirically_contingent).
narrative_ontology:cs_reference_frame('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', intellectual_property_maximization_via_uniform_patent_standards).
narrative_ontology:cs_drift_state('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', contemporary_pandemic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6e61d2ce-a08e-4b29-aff7-01c3fc2b4e2f', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_corporations).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_country_patent_holders).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_patients).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developing_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufacture branded drugs in high-income markets and recoup R&D costs through patent-protected pricing. The strong exclusivity reading guarantees their international monopoly periods are enforceable across most jurisdictions via TRIPS dispute settlement. They actively lobby for narrow compulsory licensing interpretation and actively defend their patents through litigation and trade pressure. Exit for them means abandoning the profitable enforcement architecture.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_corporations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_corporations, agenda_setter).

% Hold patent portfolios across multiple jurisdictions. The strong exclusivity reading enforces their domestic monopolies globally by constraining parallel import and compulsory licensing as narrow exceptions. They benefit from being able to price-discriminate across markets while preventing arbitrage.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_country_patent_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Interprets TRIPS text in dispute panels and appellate review. The strong exclusivity reading reflects how panels have consistently sided with narrow compulsory licensing grounds and broad patent term protection in disputes between developed and developing nations. The mechanism actively enforces this reading through trade retaliation authority.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_mechanism, agenda_setter,
    institutional, generational, analytical, global).

% Their pharmaceutical and biotechnology sectors benefit from globally enforced patents. They host the multinational corporations that profit. They set TRIPS negotiating positions and dominate the WTO dispute settlement system. They can threaten trade retaliation to enforce their preferred reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_country_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Face patent-protected drug prices that exceed their income many times over. They cannot purchase branded drugs. They depend on generic manufacturers' ability to produce affordable copies, which the strong exclusivity reading constrains. Exit from the system (non-treatment) is the only option for those without resources. They are geographically trapped and have no voice in patent policy.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_patients, payer,
    powerless, immediate, trapped, global).

% Cannot manufacture life-saving drugs under patent due to narrow compulsory licensing grounds under the strong exclusivity reading. They cannot formally invoke exceptions without facing trade retaliation through WTO dispute settlement. Their domestic generic manufacturing capacity is legally suppressed. Exiting TRIPS is theoretically possible but economically ruinous due to trade sanctions and market access loss.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_countries, payer,
    moderate, biographical, constrained, global).

% Produce affordable generic drugs in low-income countries for domestic supply and export to similar-income nations. The strong exclusivity reading interprets patent scope broadly and compulsory licensing grounds narrowly, restricting their ability to legally manufacture patented drugs even when public health crises demand it. They face legal and trade barriers to accessing patented formulations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers, payer,
    organized, biographical, constrained, national).

% Want to use compulsory licensing and parallel imports to lower drug prices for their populations but face dispute settlement risk and threat of trade sanctions if they invoke exceptions too broadly. The strong exclusivity reading constrains their policy space. They have limited ability to retaliate through the dispute system due to weak bargaining position.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developing_country_governments, payer,
    moderate, biographical, constrained, national).

% Argue for broad compulsory licensing and parallel import rights to serve public health access but are excluded from TRIPS treaty negotiations and WTO dispute panels. They have no formal voice in the interpretive authority that decides how the reading is applied. Their expertise on access outcomes is not systematically solicited.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates_and_ngos, excluded,
    organized, biographical, constrained, global).

% Study the interpretive history of TRIPS and dispute outcomes. They produce scholarship documenting how the strong exclusivity reading has consolidated through case law despite textual ambiguity. They can influence policy debate but have no binding authority over interpretation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, academic_intellectual_property_analysts, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_corporations).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: TRIPS coordinates global patent standards so multinational pharmaceutical corporations can rely on consistent, enforceable monopolies across markets. It solves the collective-action problem of developing countries competitively undercutting each other's patent protections and reduces transaction costs for multinational licensing.
% TRANSFER_FUNCTION: Moves monopoly rents from patients and low-income countries to multinational pharmaceutical corporations and developed-country patent holders. Transfers the cost of drug development risk onto end-users via patent-protected pricing. Transfers policy authority from elected governments to WTO dispute panels.
% ABSENT_VOICES: Low-income patients, generic drug manufacturers in developing countries, and public health advocates are structurally excluded from TRIPS negotiation and WTO dispute settlement. They would argue for broad compulsory licensing grounds, parallel import rights, and public health carve-outs but are kept out of the formal decision-making process. Developing-country governments have limited voice relative to developed countries in dispute settlement power dynamics.
% DISAPPEARANCE_RATIONALE: If the strong exclusivity reading and its enforcement mechanisms disappeared, multinational pharmaceutical corporations would face generic competition in low-income markets, drug prices would fall substantially, and developing countries could manufacture and trade generics without trade retaliation. The global pharmaceutical market would reorganize around price differentiation rather than monopoly exclusivity. Patent protection would become a tool available to developing countries as well, not a mechanism for extraction from them.
% FOUNDING_PROBLEM: Developing countries in the 1990s were manufacturing pharmaceutical drugs under compulsory licenses and importing generics without compensating developers. Multinational pharmaceutical corporations faced erosion of patent monopolies across jurisdictions, reducing incentives for innovation in diseases affecting poor populations. The founding problem was: how to create a minimum global patent standard that would make drug development investment profitable while allowing technology transfer?
% FOUNDING_PROBLEM_CORROBORATION: Developed countries and multinational pharmaceutical corporations argue the founding problem persists and justifies strict enforcement. Developing countries, public health organizations (WHO, Médecins Sans Frontières), and independent academic analysts argue the founding problem is substantially solved (antibiotics, vaccines, and many essential drugs are now generically available and affordable) and that continued strict enforcement produces access crises that outweigh innovation benefits. Legislative testimony and empirical access data from outside pharmaceutical industry sources support the claim that strict enforcement now harms public health more than it incentivizes innovation.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.81) reflects that the constraint transfers monopoly rents from patients and developing countries to multinational pharmaceutical corporations. This is not merely coordination cost — the strong exclusivity reading narrowly interprets exceptions that TRIPS text actually contains, restricting policy autonomy for developing countries. The founding problem (ensuring innovation incentives) has shifted: early innovation in antibiotics and vaccines is now generically available; current extraction defends pharmaceutical monopolies on newer drugs that treat wealthy-world diseases (cancer, cardiovascular). The measurement series show extractiveness ACCUMULATING from 1995 to 2026 as the strong exclusivity reading consolidated through dispute cases (India pharmaceutical patent cases, Thailand compulsory licensing dispute, pandemic vaccine production disputes). Theater ratio rises modestly (0.12 to 0.28) as enforcement machinery increasingly defends exclusivity rather than serving innovation function — during COVID-19, TRIPS was invoked to prevent generic vaccine and treatment production even in crisis conditions, revealing the extraction mechanism. Suppression requirement rises (0.58 to 0.72) as developing countries sought to invoke flexibilities and faced escalating trade pressure, requiring more active enforcement to maintain the reading. The leveled coercion grid shows STRUCTURAL-level accessibility collapse rising steeply (0.70 to 0.82): at the system level, alternatives to branded drug monopolies are collapsing; INDIVIDUAL-level collapse is lower (0.55 to 0.72) because patients can still access some generic drugs outside the patent system, but increasingly only in black markets or foreign jurisdictions. RESISTANCE rises at the CLASS level (0.55 to 0.62) as organized developing countries coordinate on access advocacy, but structural resistance (0.48 to 0.58) remains lower because the trade retaliation threat is asymmetric — poor countries have less to threaten developed countries with.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (multinational pharma corporations and developed-country governments) experience this constraint as legitimate innovation incentive structure — they authored it and enforce it. The payer seats (patients, developing countries, generic manufacturers) experience it as coercive extraction of monopoly rents from health crises. The WTO dispute mechanism sits as agenda-setter with power to adjudicate but is dominated by developed-country lawyers and officials, so its rulings consistently favor the strong exclusivity reading. A developing-country government facing trade retaliation for invoking compulsory licensing computes this as SNARE with no exit; a multinational pharma corporation with global patent portfolio computes this as ROPE with generous beneficiary margins. The engine should compute per-seat types reflecting these radically different structural positions. The claim (tangled_rope) reflects the analytical seat's observation that both coordination (minimum patent standard) and extraction (narrow exceptions) are present in the same structure — but the beneficiary and payer seats should compute very differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational pharmaceutical corporations: d ≈ 0.05 (full beneficiary; they set enforcement terms and receive monopoly rents with no cost). Developed-country governments: d ≈ 0.08 (strong beneficiary; they host pharma sectors and dominate dispute settlement; exit cost is reputational/trade). WTO dispute settlement: d ≈ 0.10 (structural beneficiary; institutionalizes the strong exclusivity reading and their interpretive authority). Low-income patients: d ≈ 0.95 (full target; trapped access deprivation, no alternatives, no voice). Developing-country governments: d ≈ 0.78 (strong targets; pay through constrained policy autonomy; exit via TRIPS withdrawal is economically ruinous). Generic manufacturers: d ≈ 0.82 (strong targets; legally suppressed production, constrained market access). Public health advocates: d ≈ 0.5 (symmetric; neither collecting nor bearing direct costs, but excluded from authority structure). The engine's directionality derivation should produce these structures automatically from the beneficiary/victim + power + exit declarations without override, except: developed-country governments might require a small upward override from their structural derivation (they have some exit cost via trade compliance, which might push them toward 0.12 rather than 0.08), and public health advocates should override to reflect analytical-seat neutrality (d=0.5 baseline).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was pharmaceutical innovation incentives in an era when developing countries were freely copying patents. TRIPS solved this coordination problem in the 1990s–2000s. However, by 2010–2026, the founding problem status shifted: (1) most essential drugs are now generic and affordable (antibiotics, basic antiretrovirals, vaccines); (2) innovation now concentrates on wealthy-world diseases (cancer, rare diseases); (3) access crises (COVID-19, monkeypox, drug-resistant TB) reveal that strict patent enforcement prevents life-saving generic production even in emergencies. The founding problem is DEAD for essential drugs and CONTESTED for newer pharmaceuticals. Yet the strong exclusivity reading persists through dispute settlement enforcement and trade pressure, increasingly serving extraction rather than innovation incentive. The constraint exhibits MANDATROPHY: the justifying function has atrophied, but the enforcement structure persists. Theater ratio rising from 0.12 to 0.28 captures this: enforcement activity increasingly defends exclusivity itself rather than supporting the original innovation incentive function. A public health reading would score the same foundational structure as SNARE, not tangled_rope, because no genuine coordination survives once the founding problem is dead; what remains is pure extraction defended by trade threat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Is pharmaceutical innovation incentive still the dominant constraint on drug development, or has the founding problem substantially shifted to access, equity, and distribution mechanisms?',
    'Comparative analysis of R&D investment patterns and drug development pipeline: are new drugs being developed in response to patent protection or to market size/wealthy-world disease burden? Does lifting patent protection in low-income countries reduce innovation, or does it reduce only extracted rents while innovation continues?',
    'If access is now the dominant constraint (as pandemic experience suggests), the founding problem is dead and the strong exclusivity reading becomes pure extraction; the constraint reclassifies from tangled_rope toward snare. If innovation incentive remains dominant, the reading retains its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Whether the founding problem (innovation incentive) persists or has atrophied.').

omega_variable(
    compulsory_licensing_textual_scope,
    'Does TRIPS Article 31 textually authorize broad compulsory licensing for public health (as the flexibility reading claims) or restrict it to narrow enumerated grounds (as the strong exclusivity reading claims)?',
    'Linguistic analysis of Article 31 text: does ''other circumstances of extreme urgency'' or ''public health crisis'' encompass persistent drug unavailability and pricing crises, or only acute emergencies? Does the DOHA Declaration''s public health emphasis override the narrower article language? A generalist linguist and a treaty expert from outside the pharmaceutical industry could adjudicate the textual scope.',
    'If Article 31 is textually broad, the strong exclusivity reading is a contested interpretation, not the text''s plain meaning; the reading''s authority grounds in dispute settlement power, not textual clarity. If Article 31 is textually narrow, the reading''s authority is grounded in accurate textual construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compulsory_licensing_textual_scope, conceptual, 'Whether the TRIPS text itself supports broad or narrow compulsory licensing scope.').

omega_variable(
    dispute_settlement_authority_legitimacy,
    'Is the WTO dispute settlement mechanism''s binding interpretive authority over TRIPS grounded in legitimate international consent and procedure, or has it consolidated through institutional power imbalance and weak participation from developing countries?',
    'Audit of dispute participation: what percentage of TRIPS disputes are initiated by developed vs. developing countries? What percentage of successful defenses are by developed countries? Do developing countries have equal access to dispute funding, legal expertise, and technical analysis? Do dispute panel appointments favor developed-country perspectives?',
    'If the mechanism is procedurally balanced, the strong exclusivity reading''s authority is grounded in legitimate dispute settlement. If developed countries dominate dispute initiation, success, and appointments, the reading''s persistence reflects institutional power imbalance, and the mechanism''s authority should be questioned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_settlement_authority_legitimacy, empirical, 'Whether WTO dispute settlement reflects balanced international consent or power imbalance favoring developed countries.').

omega_variable(
    extraction_mechanism_identity_fusion,
    'To what extent do pharmaceutical executives and developed-country officials fuse their institutional identity with patent protection doctrine, making them resistant to evidence that strict enforcement now harms innovation more than it helps?',
    'Structural analysis: examine whether officials who have spent careers defending strong patent positions change their positions when confronted with evidence (access crises, innovation patterns, public health outcomes), or whether they defend the position despite contradictory evidence. Track whether alternative readings are seriously considered in policymaking circles or reflexively dismissed.',
    'If strong identity fusion exists, the constraint will persist despite evidence that the founding problem is dead — reclassifying toward piton (inertial performance) rather than functional tangled_rope. If officials are evidence-responsive, the constraint could rebalance toward flexibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_identity_fusion, conceptual, 'Whether strong exclusivity doctrine is defended through institutional identity lock-in or evidence-based assessment.').

omega_variable(
    reading_alternative_foreclosure,
    'Can a single WTO-member state coherently adopt the public health flexibility reading while others adopt strong exclusivity, or does the binding nature of TRIPS dispute rulings foreclose the flexibility reading within the international trade system?',
    'Analysis of TRIPS binding effects: do dispute panel rulings create precedent that forecloses alternative interpretations for all WTO members, or do they bind only the disputing parties? Can a developing country use compulsory licensing broadly without facing retaliation if the dispute panel would uphold their position?',
    'If dispute rulings globally foreclose the flexibility reading (de facto single interpretation enforced via trade pressure), the readings genuinely foreclose each other within the system. If readings can coexist with different parties holding different positions, they coexist. This affects the cs_structure.reading_relations value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_alternative_foreclosure, conceptual, 'Whether WTO dispute system forecloses the public health flexibility reading or allows coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(trip_tr_t2006, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(trip_tr_t2012, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(trip_tr_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2019, 0.25).
narrative_ontology:measurement(trip_tr_t2026, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(trip_be_t2006, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2006, 0.72).
narrative_ontology:measurement(trip_be_t2012, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2012, 0.76).
narrative_ontology:measurement(trip_be_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2019, 0.79).
narrative_ontology:measurement(trip_be_t2026, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2001, 0.62).
narrative_ontology:measurement(trip_su_t2006, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2006, 0.65).
narrative_ontology:measurement(trip_su_t2012, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement(trip_su_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(trip_su_t2026, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2026, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1995, tn=2026
narrative_ontology:measurement(trip_grid_01, trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse(class), 1995, 0.62).
narrative_ontology:measurement(trip_grid_02, trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse(class), 2026, 0.78).
narrative_ontology:measurement(trip_grid_03, trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse(individual), 1995, 0.55).
narrative_ontology:measurement(trip_grid_04, trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse(individual), 2026, 0.72).
narrative_ontology:measurement(trip_grid_05, trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse(organizational), 1995, 0.48).
narrative_ontology:measurement(trip_grid_06, trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse(organizational), 2026, 0.65).
narrative_ontology:measurement(trip_grid_07, trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse(structural), 1995, 0.7).
narrative_ontology:measurement(trip_grid_08, trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse(structural), 2026, 0.82).
narrative_ontology:measurement(trip_grid_09, trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance(class), 1995, 0.55).
narrative_ontology:measurement(trip_grid_10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance(class), 2026, 0.62).
narrative_ontology:measurement(trip_grid_11, trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance(individual), 1995, 0.38).
narrative_ontology:measurement(trip_grid_12, trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance(individual), 2026, 0.32).
narrative_ontology:measurement(trip_grid_13, trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance(organizational), 1995, 0.52).
narrative_ontology:measurement(trip_grid_14, trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance(organizational), 2026, 0.48).
narrative_ontology:measurement(trip_grid_15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance(structural), 1995, 0.48).
narrative_ontology:measurement(trip_grid_16, trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance(structural), 2026, 0.58).
narrative_ontology:measurement(trip_grid_17, trips_agreement_interpretive_kernel__strong_exclusivity_reading, stakes_inflation(class), 1995, 0.6).
narrative_ontology:measurement(trip_grid_18, trips_agreement_interpretive_kernel__strong_exclusivity_reading, stakes_inflation(class), 2026, 0.74).
narrative_ontology:measurement(trip_grid_19, trips_agreement_interpretive_kernel__strong_exclusivity_reading, stakes_inflation(individual), 1995, 0.58).
narrative_ontology:measurement(trip_grid_20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, stakes_inflation(individual), 2026, 0.76).
narrative_ontology:measurement(trip_grid_21, trips_agreement_interpretive_kernel__strong_exclusivity_reading, stakes_inflation(organizational), 1995, 0.44).
narrative_ontology:measurement(trip_grid_22, trips_agreement_interpretive_kernel__strong_exclusivity_reading, stakes_inflation(organizational), 2026, 0.58).
narrative_ontology:measurement(trip_grid_23, trips_agreement_interpretive_kernel__strong_exclusivity_reading, stakes_inflation(structural), 1995, 0.66).
narrative_ontology:measurement(trip_grid_24, trips_agreement_interpretive_kernel__strong_exclusivity_reading, stakes_inflation(structural), 2026, 0.8).
narrative_ontology:measurement(trip_grid_25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression(class), 1995, 0.58).
narrative_ontology:measurement(trip_grid_26, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression(class), 2026, 0.71).
narrative_ontology:measurement(trip_grid_27, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression(individual), 1995, 0.52).
narrative_ontology:measurement(trip_grid_28, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression(individual), 2026, 0.68).
narrative_ontology:measurement(trip_grid_29, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression(organizational), 1995, 0.61).
narrative_ontology:measurement(trip_grid_30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression(organizational), 2026, 0.74).
narrative_ontology:measurement(trip_grid_31, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression(structural), 1995, 0.6).
narrative_ontology:measurement(trip_grid_32, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression(structural), 2026, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.18).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_dispute_settlement_authority).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_compulsory_licensing_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS kernel; the public_health_flexibility_reading is the sibling reading instantiating the same text from a different authority perspective. The strong exclusivity reading emphasizes multinational pharma beneficiary structure and narrow exceptions; the flexibility reading emphasizes patient/access beneficiary structure and broad exceptions. They share the same referent (TRIPS text) but arrive at opposite ε values (strong exclusivity: ε≈0.81 extraction; flexibility would score ε≈0.35). The difference is not measurement; it is interpretation. They are structurally distinct constraints linked by kernel kinship, not two views of the same constraint. The disputes_settlement_authority constraint (sister story, separate kernel) tracks WTO's institutional power to enforce whichever reading dominates; affects_constraints arrows point downstream because the dispute settlement reading determines which TRIPS reading is binding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
