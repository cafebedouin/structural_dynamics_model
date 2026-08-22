% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__market_access_reading, []).

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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Treaty Framework — Market-Access Reading (Symmetric Liberalization Obligation)
 *   domain: international_trade_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the wto_treaty_framework kernel:
 *   the market_access_reading, under which trade liberalization is a
 *   symmetric universal obligation, non-discrimination and market access are
 *   the treaty's primary purpose, and Special & Differential Treatment
 *   provisions are temporary transitional exceptions rather than permanent
 *   structural accommodation. The ε referent is the standing arrangement
 *   under contest — the covered agreements as administered under this
 *   symmetric-obligation interpretation — assessed by this reading's own
 *   lights: measured against its own standard of symmetry, the arrangement
 *   disciplines the instruments developing members would use (tariffs,
 *   subsidies, local content) far more tightly than it disciplines the
 *   entrenched supports of the largest traders, and the burden of adjustment
 *   runs downhill. The sibling developmental_reading is a DIFFERENT
 *   constraint with a different victim set and materially lower extraction
 *   for developing-country seats; it is linked as a family member via
 *   network.affects_constraints and is not averaged into this file.
 *   Constraint family: the colloquial object 'the WTO trade regime'
 *   decomposes into these two readings because a single story cannot carry
 *   one stable ε across them — the market-access reading currently dominates
 *   dispute-settlement jurisprudence and thus shapes the developmental
 *   reading's operating environment, but the two remain live competing
 *   positions held by different member coalitions.
 *
 * KEY AGENTS:
 *   - major_trading_powers: Agenda-setter (institutional/arbitrage) — drafted the symmetric-obligation architecture, steer consensus, retain the largest exempted supports
 *   - wto_dispute_settlement_system: Agenda-setter (institutional/analytical) — administers and adjudicates; its jurisprudence defines what the text means; capacity currently impaired
 *   - multinational_corporations: Primary beneficiary (institutional/arbitrage) — collect enforceable market access and freedom from local-content mandates; can relocate across jurisdictions
 *   - advanced_economy_export_sectors: Beneficiary (organized/mobile) — sell into opened markets while home markets retain carve-outs
 *   - developing_country_governments: Payer with secondary beneficiary position (organized/constrained) — gain guaranteed export access, lose tariff/subsidy/local-content instruments; coalition-based counter-leverage only
 *   - infant_industries_in_developing_countries: Payer (powerless/trapped) — barred from the protective toolkit every earlier industrializer used at their stage
 *   - smallholder_farmers_in_developing_countries: Payer (powerless/trapped) — absorb subsidized import competition without storage, credit, or crop-switching capacity
 *   - informal_sector_workers_in_adjusting_regions: Excluded (powerless/trapped) — absorb import-shock adjustment with no seat and no standing
 *   - unctad_development_economists: Observer (analytical/analytical) — document the symmetry/capacity divergence; supply the evidentiary record other seats cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.74).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.52).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Treaty Framework — Market-Access Reading (Symmetric Liberalization Obligation)").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, '2213b858-ef97-4336-8b36-892d784fb699').
narrative_ontology:cs_kernel_codification('2213b858-ef97-4336-8b36-892d784fb699', fixed_text).
narrative_ontology:cs_authority_grounding('2213b858-ef97-4336-8b36-892d784fb699', lineage).
narrative_ontology:cs_interpretation_layer_present('2213b858-ef97-4336-8b36-892d784fb699').
narrative_ontology:cs_reading_relation('2213b858-ef97-4336-8b36-892d784fb699', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('2213b858-ef97-4336-8b36-892d784fb699', foundational, nondiscrimination_as_primary_treaty_purpose).
narrative_ontology:cs_axiom_status(nondiscrimination_as_primary_treaty_purpose, holdable).
narrative_ontology:cs_axiom_grounding('2213b858-ef97-4336-8b36-892d784fb699', nondiscrimination_as_primary_treaty_purpose, conventional).
narrative_ontology:cs_axiom('2213b858-ef97-4336-8b36-892d784fb699', foundational, special_differential_treatment_temporary_by_design).
narrative_ontology:cs_axiom_status(special_differential_treatment_temporary_by_design, holdable).
narrative_ontology:cs_axiom_grounding('2213b858-ef97-4336-8b36-892d784fb699', special_differential_treatment_temporary_by_design, instrumental).
narrative_ontology:cs_reference_frame('2213b858-ef97-4336-8b36-892d784fb699', symmetric_reciprocal_liberalization_compact).
narrative_ontology:cs_drift_state('2213b858-ef97-4336-8b36-892d784fb699', post_doha_fragmentation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2213b858-ef97-4336-8b36-892d784fb699', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, advanced_economy_export_sectors).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries_in_developing_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, smallholder_farmers_in_developing_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, developing_country_governments).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, mfn_nondiscrimination_principle).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, single_undertaking_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the covered agreements and continue to steer agendas through consensus management and restricted pre-negotiation meetings. Maintain the largest exempted support programs — agricultural subsidies running well above the allowances available to poorer members — while pressing uniform discipline on everyone else. If dissatisfied with the framework, they can route commerce through plurilateral arrangements or unilateral measures, giving them usable outside options most members lack.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, major_trading_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Adjudicates complaints that members breached scheduled commitments and issues rulings backed by authorized retaliation. Its caseload and jurisprudence define what the treaty text means in day-to-day operation. Its capacity depends on seated adjudicators; the appointment blockage since 2019 has thinned its appellate tier and left some appeals suspended in limbo.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_dispute_settlement_system, agenda_setter,
    institutional, generational, analytical, global).

% Operate integrated production and distribution networks across many jurisdictions. Gain enforceable access to member markets, freedom from local-content mandates, and predictable tariff ceilings they price into long-horizon supply contracts. Can shift production locations and profit booking across borders to optimize tax and tariff exposure — an outside option unavailable to domestically rooted competitors.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Sell capital goods, services, and branded manufactures into markets opened by scheduled commitments, and organize persistently through industry associations to press for deeper schedules. Their home markets retain carve-outs in agriculture and sensitive services that their exporting counterparts in poorer members cannot match.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, advanced_economy_export_sectors, beneficiary,
    organized, biographical, mobile, global).

% Accepted uniform obligations covering precisely the instruments they had planned to use for industrialization. They gain guaranteed access for their competitive exports, but bear implementation costs, litigation expenses few can afford, and the loss of tariff, subsidy, and local-content tools. They counter-leverage through negotiating coalitions (G20, G90, African Group) and press for extended accommodation; leaving the framework would forfeit market access entirely and mark them as outside the trading system, so exit is materially closed even where legally available.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_governments, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, developing_country_governments, beneficiary).

% Firms in the early stages of building manufacturing and technological capability. The treaty floor bars the tariff protection, performance requirements, and local-content support that every earlier industrializer deployed at this stage of formation. They compete from birth against incumbents operating at global scale; relocation is not an option because their capability is rooted in place, and they are too young and dispersed to litigate or lobby effectively.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries_in_developing_countries, payer,
    powerless, generational, trapped, national).

% Grow staple crops alongside import flows from producers supported by subsidies far exceeding the allowances available to their own governments. They lack the storage, credit, and marketing infrastructure to switch crops or reach alternative markets, so world-price swings and import surges transmit directly to household income.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, smallholder_farmers_in_developing_countries, payer,
    powerless, biographical, trapped, local).

% Work in regions hit by import surges and plant closures following liberalization commitments. They hold no seat in negotiations, no standing in disputes, and rely on thin safety nets; adjustment assistance reaches them rarely and late, and moving to where new jobs appear requires resources they do not have.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, informal_sector_workers_in_adjusting_regions, excluded,
    powerless, immediate, trapped, local).

% Analyze the distribution of treaty burdens across income levels and document the divergence between formal symmetry and material capacity. They publish through UNCTAD and academic channels, hold no vote, and supply the evidentiary record that negotiating coalitions and litigants cite.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, unctad_development_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates reciprocal market opening among sovereign states: most-favored-nation non-discrimination prevents discriminatory-bloc spirals, tariff bindings stabilize expectations, scheduled commitments make openings credible, and rule-based dispute settlement substitutes adjudication for power-weighted retaliation — a collective-action solution no pair of states achieves bilaterally at scale.
% TRANSFER_FUNCTION: Moves policy discretion and market surplus: transfers tariff-setting, subsidy, and local-content authority from all member governments down to the treaty floor; moves consumer surplus and procurement access toward globally mobile firms and competitive exporters; and because obligations are uniform while capacities are not, the net flow of adjustment burden runs from advanced economies toward developing ones.
% ABSENT_VOICES: Informal-sector workers absorbing import shocks, would-be industrial firms that do not yet exist and therefore cannot petition, subsistence producers outside export circuits, and delegations without Geneva legal staff — all structurally outside restricted agenda-setting sessions and priced out of dispute-settlement litigation. Their absence is commentary-grade: unanimity at ministerials partly reflects who was never in the room.
% DISAPPEARANCE_RATIONALE: Tariff schedules, subsidy programs, and procurement rules calibrated to the treaty floor would unwind; trade would reorganize around bilateral power bargaining, rival bloc standards, and retaliatory cycles; the dispute docket, the Secretariat's administration, and the market-access expectations priced into global supply chains would all dissolve. Regional agreements would partially backfill, but the current configuration demonstrably depends on the framework — which is exactly why its erosion is fought so hard by some seats and welcomed by others.
% FOUNDING_PROBLEM: Interwar beggar-thy-neighbor tariff escalation that deepened the Great Depression; the postwar designers sought to lock in reciprocal liberalization against recurring domestic protectionist cycles and to prevent discriminatory trade blocs from hardening into political rivalry.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by economic historians of the interwar collapse, IMF and OECD analyses of fragmentation costs, and import-dependent manufacturers testifying to the value of schedule stability. The multinational and exporter seats also attest the founding problem, but their attestation alone would be cover-story-prone; the external attestations are what distinguish a live problem from a retained justification.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__market_access_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at interval end) because obligations are uniform while capacities are not: the treaty floor removes tariff, subsidy, and local-content tools precisely where they are the scarce instruments of catch-up growth, while the largest traders' agriculture and services carve-outs persist above developing-country allowance levels. Suppression (0.52) is a raw structural property — unscaled by power or scope; the engine scales only extractiveness — and reflects enforcement that is real but currently impaired: compliance pressure runs through authorized retaliation and market-access dependence rather than direct coercion, and the appellate tier has been paralyzed since 2019. Theater ratio (0.35) is driven mostly by hortatory S&D language ('best endeavours', expiring waivers), the long stagnation of the development round, and ministerial declaration inflation — the coordination core (bindings, schedules, dispute settlement) remains functional. Accessibility collapse is moderate (0.48): alternatives exist (regional agreements, plurilaterals, autarkic retrenchment) but each is costlier than the constraint it replaces, so alternatives narrow without vanishing. Resistance is substantial (0.6): the collapsed development round, G20/G90 coalition formation, food-security standoffs, the appointment blockage, and the broader unilateral turn all register as active pushback. The measurement series run on one shared seven-point grid so every metric is authored at every examined time point. The suppression_requirement series deliberately traces the enforcement-capacity arc — build-up through the activist adjudication era (peak ~0.66 at midpoint), then decay as appointments stalled — which is why it is authored as a series rather than left to the static scalar; the scalar matches the series endpoint. Extractiveness rises monotonically as S&D erodes and policy-space compression accumulates; no oscillation mechanism is asserted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine derives that divergence from the structural data. From the major-trading-powers seat, the arrangement is a rules-based order they authored: obligations bind others more than themselves, outside options abound, and the experienced type leans toward coordination they built and police. From the infant-industry and smallholder seats, the same structure operates as one-way discipline: binding obligations without countervailing capacity, trapped exit, and no seat at agenda-setting — the experienced type leans toward enforced extraction. The developing-government seat straddles: guaranteed access for competitive exports (real coordination gain) against surrendered instruments and litigation costs (real extraction), which is why it is authored payer-first with a secondary beneficiary role. Same-power divergence is visible at the 'organized' atom: advanced_economy_export_sectors and developing_country_governments hold the same power atom but diverge through role and exit (mobile beneficiary versus constrained payer), so the derivation separates them without intervention.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: multinational_corporations combine beneficiary position with arbitrage-grade exit (production and booking mobility across jurisdictions), placing them nearest the beneficiary end; advanced_economy_export_sectors are mobile beneficiaries. Victim declarations map to high-directionality seats: infant industries and smallholder farmers are trapped targets whose entire predicament is rooted-in-place exposure to the obligation floor. Developing_country_governments are the deliberate dual-positioned seat — payer primary, beneficiary secondary — with a net target-leaning position because surrendered instruments outweigh access gains under this reading's symmetric-obligation operation. No directionality_overrides are authored: role plus exit differentiation already separates the same-power-atom seats, and the derivation chain handles the dual-role government seat through its primary payer declaration. No seat is authored identity_locked: developing governments carry a reputational attachment to membership-as-respectability, but the binding exit factor is material market-access dependence, hence constrained rather than identity_locked.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards against both mislabels. Reading the arrangement as pure coordination (rope) ignores the named victims and the enforcement requirement — the asymmetry is not incidental friction but the operating mode: uniform rules over unequal capacities move adjustment burden downhill. Reading it as pure extraction (snare) ignores the genuine collective-action function: MFN non-discrimination prevents discriminatory-bloc spirals, bindings stabilize expectations that global supply chains price in, and adjudication substitutes for power-weighted retaliation in a way no bilateral alternative replicates at scale — alternatives are narrowed, not suppressed outright. The founding problem (interwar tariff-war dynamics) remains live among the great powers, which keeps the constraint from piton territory: parties still collect enough from its operation to defend it actively. Theater concentrates in the S&D hortatory text rather than the whole apparatus, holding theater_ratio moderate rather than high. The receipt surface records the capture (multinational_corporations) and the cost class of repair (prohibitive: consensus rule plus single undertaking means any rebalancing requires the consent of the seats the current distribution favors — twenty years of development-round stagnation is the demonstrated price).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the wto_treaty_framework kernel — the market_access_reading. The sibling developmental_reading instantiates a different constraint from the same treaty text: S&D as permanent structural accommodation, technology transfer as core commitment, policy space as equal-status obligation. Which reading governs interpretation materially changes the victim set and the burden distribution.',
    'Track which reading dominates dispute-settlement jurisprudence, negotiation outcomes, and S&D review decisions over successive ministerial conferences.',
    'If the developmental reading gains interpretive authority, S&D hardens into permanent differentiated obligation, the infant-industry and smallholder victim seats lighten, and effective extraction for developing-country seats drops substantially; if the market-access reading consolidates, the current profile persists or deepens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the WTO kernel governs, and what each would change structurally.').

omega_variable(
    formal_symmetry_vs_material_capacity,
    'Does uniform obligation across unequally situated members constitute fairness (this reading''s own premise), or does equal treatment of unequal parties constitute a burden transfer? The market-access reading evaluates the arrangement against its own symmetry standard — the answer determines whether the measured extraction is intrinsic to the design or an artifact of capacity gaps the design refuses to price.',
    'Comparative accounting of implementation and compliance costs per unit of obligation across member income levels, controlling for sector composition.',
    'If compliance cost scales inversely with capacity, the symmetric-obligation premise is doing extraction work and the constraint''s coordination claim narrows to the advanced-economy core; if costs are roughly proportional, part of the measured extraction is misattributed capacity difference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_symmetry_vs_material_capacity, conceptual, 'Whether formal symmetry is fairness or a vehicle for asymmetric burden.').

omega_variable(
    sdt_graduation_empirics,
    'Is the ''temporary transitional exception'' framing of Special & Differential Treatment empirically transitional — do members actually graduate out of S&D eligibility upon reaching development thresholds — or does the temporary label function to indefinitely defer structural accommodation?',
    'Graduation data: count members that have exited S&D categories, at what income levels, and whether graduation was self-declared or externally forced.',
    'If graduation is rare and externally resisted, the transitional justification collapses toward indefinite deferral, the theater share attributable to S&D text rises, and this reading''s own warrant for the arrangement weakens on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sdt_graduation_empirics, empirical, 'Whether S&D temporariness is real transition or deferred permanence.').

omega_variable(
    appellate_body_paralysis_trajectory,
    'Is the current enforcement decay (Appellate Body appointment blockage since 2019, appeals into the void) terminal — the constraint drifting toward inertial maintenance — or cyclical, with reform restoring adjudicative capacity?',
    'Observe whether appointment impasse resolves, whether a replacement arbitration mechanism (MPIA-style) absorbs the docket, and whether compliance rates for adverse rulings hold without appellate review.',
    'Terminal decay pushes the constraint toward theatrical persistence with falling suppression requirement; restored enforcement re-ratchets suppression upward and re-concentrates discipline on members least able to retaliate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_paralysis_trajectory, empirical, 'Whether enforcement decline is terminal attrition or a reformable trough.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_treaty_framework__market_access_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(wto__tr_t0, observed).
narrative_ontology:measurement(wto__tr_t5, wto_treaty_framework__market_access_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(wto__tr_t5, observed).
narrative_ontology:measurement(wto__tr_t10, wto_treaty_framework__market_access_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(wto__tr_t10, observed).
narrative_ontology:measurement(wto__tr_t15, wto_treaty_framework__market_access_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(wto__tr_t15, observed).
narrative_ontology:measurement(wto__tr_t20, wto_treaty_framework__market_access_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(wto__tr_t20, observed).
narrative_ontology:measurement(wto__tr_t25, wto_treaty_framework__market_access_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(wto__tr_t25, observed).
narrative_ontology:measurement(wto__tr_t30, wto_treaty_framework__market_access_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(wto__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_treaty_framework__market_access_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(wto__be_t0, observed).
narrative_ontology:measurement(wto__be_t5, wto_treaty_framework__market_access_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(wto__be_t5, observed).
narrative_ontology:measurement(wto__be_t10, wto_treaty_framework__market_access_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(wto__be_t10, observed).
narrative_ontology:measurement(wto__be_t15, wto_treaty_framework__market_access_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(wto__be_t15, observed).
narrative_ontology:measurement(wto__be_t20, wto_treaty_framework__market_access_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(wto__be_t20, observed).
narrative_ontology:measurement(wto__be_t25, wto_treaty_framework__market_access_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(wto__be_t25, observed).
narrative_ontology:measurement(wto__be_t30, wto_treaty_framework__market_access_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement_basis(wto__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_treaty_framework__market_access_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(wto__su_t0, observed).
narrative_ontology:measurement(wto__su_t5, wto_treaty_framework__market_access_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(wto__su_t5, observed).
narrative_ontology:measurement(wto__su_t10, wto_treaty_framework__market_access_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(wto__su_t10, observed).
narrative_ontology:measurement(wto__su_t15, wto_treaty_framework__market_access_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(wto__su_t15, observed).
narrative_ontology:measurement(wto__su_t20, wto_treaty_framework__market_access_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(wto__su_t20, observed).
narrative_ontology:measurement(wto__su_t25, wto_treaty_framework__market_access_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(wto__su_t25, observed).
narrative_ontology:measurement(wto__su_t30, wto_treaty_framework__market_access_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(wto__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, resource_allocation).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the ε-invariance principle: the colloquial label 'the WTO trade regime' conflates two structurally distinct claims that measure differently. The market-access reading (this file) authors high ε against a victim set of infant industries, smallholder farmers, and instrument-stripped developing governments, with S&D as temporary exception. The developmental reading (sibling file) authors materially lower ε for developing-country seats — S&D as permanent accommodation converts much of the measured extraction into recognized differential obligation — and adds technology-transfer commitments absent here. The upstream story is the market-access reading: it currently dominates DSM jurisprudence and agenda-setting, so it shapes the environment in which the developmental reading operates; the sibling file should carry the reciprocal edge. Each story keeps one stable ε, its own stakeholders, and its own axioms; neither averages over the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
