% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework — Developmental Reading (Policy Space as Equal-Status Commitment)
 *   domain: international trade law / development economics / political economy
 *
 * SUMMARY:
 *   This story instantiates the DEVELOPMENTAL READING of the WTO treaty
 *   kernel: policy space for development is an equal-status treaty
 *   commitment, special-and-differential treatment is a permanent structural
 *   accommodation recognizing asymmetric starting conditions, and technology
 *   transfer is a core commitment rather than a courtesy. Under this reading
 *   the treaty order preserves tariff flexibility, subsidy space, and
 *   compulsory-licensing authority; infant industries are protected; Southern
 *   states are primary beneficiaries; multinational IP rights are bounded by
 *   transfer obligations and licensing authority. Epsilon's referent is this
 *   standing arrangement — the treaty order as the developmental reading
 *   construes it — assessed by the reading's own lights; the sibling
 *   market-access reading is a separate constraint in a separate file and is
 *   never averaged into this one. KEY AGENTS (by structural relationship): -
 *   developing_country_governments: Primary beneficiary
 *   (organized/constrained) — holds tariff headroom, subsidy space, licensing
 *   authority - least_developed_country_members: Deepest beneficiary
 *   (powerless/trapped) — longest transitions, least capacity to deploy the
 *   space - infant_industry_producers: Protected-sector beneficiary
 *   (moderate/constrained) - generic_pharmaceutical_manufacturers:
 *   Licensing-enabled beneficiary (organized/constrained) -
 *   multinational_ip_rights_holders: Primary target (institutional/arbitrage)
 *   — exclusivity bounded by licensing and transfer pressure -
 *   developed_country_exporters: Secondary target (powerful/constrained) —
 *   faces protected Southern markets - developing_country_consumers:
 *   Dual-positioned seat (benefit over time, protected prices now) -
 *   smallholder_farming_households: Excluded seat — absorbs incidence without
 *   voice - wto_dispute_settlement_body: Administers and enforces the
 *   actionable half; cannot compel the accommodation half -
 *   trade_and_development_analysts: Analytical observer — sees the full
 *   structure, binds nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.49).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.4).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.49).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework — Developmental Reading (Policy Space as Equal-Status Commitment)").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international trade law / development economics / political economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, '512f9070-ad4a-4475-b84e-444cd5cf774c').
narrative_ontology:cs_kernel_codification('512f9070-ad4a-4475-b84e-444cd5cf774c', fixed_text).
narrative_ontology:cs_authority_grounding('512f9070-ad4a-4475-b84e-444cd5cf774c', lineage).
narrative_ontology:cs_interpretation_layer_present('512f9070-ad4a-4475-b84e-444cd5cf774c').
narrative_ontology:cs_reading_relation('512f9070-ad4a-4475-b84e-444cd5cf774c', wto_treaty_framework__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('512f9070-ad4a-4475-b84e-444cd5cf774c', foundational, policy_space_equal_status_commitment).
narrative_ontology:cs_axiom_status(policy_space_equal_status_commitment, holdable).
narrative_ontology:cs_axiom_grounding('512f9070-ad4a-4475-b84e-444cd5cf774c', policy_space_equal_status_commitment, conventional).
narrative_ontology:cs_axiom('512f9070-ad4a-4475-b84e-444cd5cf774c', foundational, permanent_structural_accommodation_for_asymmetric_starts).
narrative_ontology:cs_axiom_status(permanent_structural_accommodation_for_asymmetric_starts, holdable).
narrative_ontology:cs_axiom_grounding('512f9070-ad4a-4475-b84e-444cd5cf774c', permanent_structural_accommodation_for_asymmetric_starts, deontological).
narrative_ontology:cs_reference_frame('512f9070-ad4a-4475-b84e-444cd5cf774c', equal_status_development_partnership).
narrative_ontology:cs_drift_state('512f9070-ad4a-4475-b84e-444cd5cf774c', contemporary_appellate_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('512f9070-ad4a-4475-b84e-444cd5cf774c', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, developing_country_governments).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_country_members).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, infant_industry_producers).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_country_exporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, developing_country_consumers).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developing_country_consumers).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, enabling_clause_differential_treatment).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, trips_public_health_flexibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate as coalitions (G77, African Group, G20 agriculture) across ministerials. Maintain bound tariff ceilings well above applied rates, run agricultural support at permitted higher levels, invoke compulsory licensing under the public-health declarations, and press the technology-transfer committees. Bound ceilings limit future escalation but leave wide day-to-day discretion; abandoning the treaty system would forfeit guaranteed market access and dispute protection, so membership is retained despite recurring grievance.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developing_country_governments, beneficiary,
    organized, generational, constrained, global).

% Receive the longest transition timelines, duty-free access promises, and extended pharmaceutical patent exemptions, but frequently lack the industrial base or administrative capacity to convert reserved space into production. Dependence on preference margins and donor budgets leaves them unable to threaten walkout credibly.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_country_members, beneficiary,
    powerless, generational, trapped, national).

% Domestic manufacturers in larger Southern economies operate behind tariff walls and directed credit, building scale before facing import competition. Their viability depends on continued national discretion over rates and subsidies; sectors that mature sometimes join calls to preserve that discretion for successors.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, infant_industry_producers, beneficiary,
    moderate, biographical, constrained, national).

% Produce off-patent and compulsorily licensed medicines, supplying domestic health systems and exporting under the paragraph-6 waiver arrangements. Their business models depend on patent exclusivity remaining bounded inside Southern jurisdictions; their advocacy seeks wider licensing scope in third markets.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, generic_pharmaceutical_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Buy food, medicine, and manufactures inside protected markets: prices run above world levels where tariffs and support schemes raise them, while industrial growth and cheaper generics return gains over time. Organized consumer voice is thin relative to producer lobbies on every side.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developing_country_consumers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, developing_country_consumers, payer).

% Hold patent, copyright, and data portfolios whose territorial strength varies with each member's implementation choices. Compulsory-license authority, the public-health declarations, and transfer-committee pressure cap what exclusivity earns in Southern markets; response runs through TRIPS-plus chapters in bilateral agreements, tiered pricing, and home-government leverage.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders, payer,
    institutional, biographical, arbitrage, global).

% Sell into Southern markets where tariff peaks, escalating schedules, and farm-support schemes raise their costs, in exchange for guaranteed access elsewhere under the same treaty. Retaliation outside agreed procedures is unavailable; influence operates through negotiation positions and domestic politics rather than unilateral action.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_country_exporters, payer,
    powerful, biographical, constrained, global).

% Grow subsistence and cash crops inside economies whose price supports, input tariffs, and import exposure are settled in councils far above them. They absorb retail-price and displacement effects of arrangements they never negotiated and hold no seat in any committee reviewing them.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, smallholder_farming_households, excluded,
    powerless, biographical, trapped, local).

% Panels and (until the recent paralysis) the Appellate Body adjudicate complaints over bindings, subsidies, and intellectual-property rules. Market-access and IP commitments are actionable and regularly litigated — developing members are frequent and often winning participants — while special-treatment and transfer clauses rest on best-endeavor wording that adjudicators have declined to convert into enforceable duties.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% University researchers, UNCTAD staff, and think-tank economists audit whether reserved space yields industrial upgrading, whether transfer clauses move capability, and how litigation outcomes distribute across memberships. Their findings circulate in ministerial corridors but bind nothing.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, trade_and_development_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of mutual trade opening with credible commitments: MFN baselines, bound tariffs, and dispute settlement give every member predictable access, while burden-sharing by development status lets members open without freezing the existing industrial hierarchy — catch-up instruments (tariff flexibility, subsidy space, licensing authority) are coordinated alongside liberalization rather than against it.
% TRANSFER_FUNCTION: Moves enforceable market-access guarantees and legal predictability to all members; moves policy autonomy (tariff headroom, subsidy rights, compulsory-licensing authority) and, on paper, technology and capability flows from developed members and rights-holders toward developing members; places the adjustment costs on exporting and IP-dependent interests in the North.
% ABSENT_VOICES: Smallholder farming households, informal-sector workers, and consumer households inside developing members absorb the price and incidence effects of tariff, subsidy, and licensing choices without seats anywhere in the process; future generations affected by lock-in of industrial paths and civil-society and ecological constituencies stand wholly outside the state-centric negotiation. They surface only through the capitals that represent them, filtered by producer-weighted domestic politics.
% DISAPPEARANCE_RATIONALE: If the developmental commitments and accommodations vanished overnight, Southern members would face fully symmetric obligations immediately: tariff shields and subsidy entitlements lapse, compulsory-licensing authority narrows to TRIPS minima, transfer clauses evaporate. Retaliatory spirals and a rush into preferential blocs would follow; the multilateral trading order would reorganize around power-weighted market size rather than rule-guaranteed access.
% FOUNDING_PROBLEM: The mid-century and post-colonial design problem: how to build an open multilateral trading order that does not permanently freeze the existing industrial hierarchy — giving late-developing states room to industrialize behind limited protection while keeping them committed to openness. Institutionalized successively in GATT Part IV (1965), the Enabling Clause (1979), and the Marrakesh Agreement preamble (1994).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Doha Development Agenda mandate was adopted by consensus including the developed members, whose own ministerial declarations attest the development objective; UNCTAD and World Bank trade-and-development assessments document persistently asymmetric starting conditions; and the scholarly literature on late industrialization attests both the founding problem and its continuing live status. Attestation is therefore not confined to the beneficiary set.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__developmental_reading, 0.49, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.49 at interval end) because the arrangement deliberately builds a transfer into the rules — that is its point under this reading — while still delivering real coordination goods: MFN predictability, bound ceilings, and a dispute forum Southern members have used successfully. Suppression (0.40, raw and unscaled — only extractiveness is scaled by directionality and scope) reflects single-undertaking lock-in offset by genuinely available alternatives: preferential tracks, GSP-style preferences, regional integration, and post-2019 enforcement decay. Theater_ratio (0.45) records a monotone Goodhart drift: a growing share of special-and-differential activity is committee monitoring and ritual Article 66.2 reporting rather than enforceable delivery, though functional adjudication keeps the ratio below dominance. The temporal series run on ONE shared seven-point grid (T0=1995 Marrakesh entry into force; unit = years; T30=2025): base_extractiveness dips after Doha (T10-T15) as the public-health declarations and licensing flexibilities gained textual force, then creeps back up as flexibilities proved slow to deploy and the waiver fights exposed limits; suppression_requirement traces an enforcement arc — ratchet up through the TRIPS phase-in deadlines (peak 0.62 at T10), plateau, then decay after the Appellate Body paralysis (0.40 at T30); theater_ratio rises steadily throughout. The arc is not cyclical; it is a hump-and-drift profile driven by identifiable treaty events, and the base_properties scalars are authored as the T30 end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute different types from the same structure. From the rights-holder seat the arrangement is an imposed asymmetry capping earned exclusivity; from the Southern-government seat it is an earned equity correction of the same rules; the dispute-settlement seat experiences a bifurcated instrument — hard law for access and IP, soft law for accommodation. Same-level differentiation matters within each nominal side: multinational IP holders (institutional power, arbitrage exit) experience the bounds very differently from developed-country exporters (powerful, constrained exit) on the payer side; and organized Southern governments deploy the accommodation that powerless least-developed members can barely use, despite both being beneficiaries. The engine computes this divergence from the structural data — power, exit, and declared position — and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (Southern governments, LDC members, infant-industry producers, generics manufacturers) drive d toward the beneficiary end for those seats; the victim declarations (rights holders, developed exporters) drive d toward the target end. Exit modulates within sides: the rights holders' arbitrage-grade exit (relocation, TRIPS-plus bilateralism, tiered pricing) sits them nearer the beneficiary end of the target range than the constrained exporters, whose lack of unilateral recourse leaves them near full-target. Least-developed members are trapped beneficiaries: structurally subsidized in d terms, but their inability to deploy the space is a capacity fact the engine reads from exit and power, not an authored perception. Developing-country consumers hold a dual role (declared on the stakeholder surface) placing them near symmetric. Smallholder households are an authored absence — commentary-grade only; per the R3 ruling their exclusion informs the absent-voices answer and never drives a classification override. Global spatial scope raises verification difficulty and thus amplifies effective extraction modestly for target-side seats; suppression receives no such scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — an open trading order that does not freeze the industrial hierarchy — remains live, and the R5 interview records it as live with corroboration from outside the beneficiary set. The classification guards both mislabelings: reading the arrangement as a pure snare would erase the genuine coordination delivered (predictability, adjudicated access, Southern litigation wins); reading it as a pure rope would erase the deliberate, targeted costs imposed on identifiable payers through the same rules. Hence the tangled-rope claim: genuine coordination function plus asymmetric extraction through one structure, held up by active enforcement of its actionable half. On the mismatch consumer, founding_problem_status=live crossed with disappearance_verdict=world_rearranges is the aligned cell — no zombie flag fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the WTO treaty kernel governs the operative commitment structure — the developmental reading instantiated here (policy space as equal-status commitment, special-and-differential treatment permanent, technology transfer obligatory) or the market_access sibling (symmetric universal obligation, special-and-differential treatment transitional)?',
    'Vienna Convention object-and-purpose analysis combined with subsequent-practice assessment: whether consolidated ministerial practice sustains Doha-type development mandates or plurilateral market-access coalitions, and how dispute settlement treats special-and-differential clauses.',
    'If the market-access sibling prevails, the same text recomputes with reversed beneficiary structure — Northern exporters become beneficiaries and Southern policy space becomes derogation — and effective extraction on Southern seats rises sharply; this story''s constraint dissolves into the sibling''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This story is one reading of the wto_treaty_framework kernel; classification is conditional on that reading holding.').

omega_variable(
    sd_binding_status,
    'Are the special-and-differential provisions legally binding commitments or best-endeavor aspirations?',
    'Systematic review of dispute-settlement jurisprudence for any ruling converting special-and-differential language into an enforceable duty, plus drafting-history analysis of the Enabling Clause and Marrakesh texts.',
    'If binding, the accommodation is substantive and the arrangement sits nearer pure coordination; if hortatory, the performative share grows and the developmental reading''s core commitment loses legal substance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_binding_status, empirical, 'Legal quality of the special-and-differential acquis underpinning this reading.').

omega_variable(
    tech_transfer_efficacy,
    'Do Article 66.2 obligations and related transfer mechanisms produce measurable capability transfer in recipient economies, or ritual reporting?',
    'Longitudinal capability audits (production depth, R&D localization, licensure of process know-how) in recipient sectors correlated with Committee on Trade and Transfer of Technology reporting activity.',
    'If efficacious, the reading''s core commitment functions and supports the coordination half of the structure; if ritual, theater share rises and the transfer axiom loses its warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tech_transfer_efficacy, empirical, 'Whether the technology-transfer commitment delivers or performs.').

omega_variable(
    policy_space_capture,
    'Does the preserved policy space finance broad-based catching-up, or concentrated rent capture by connected elites inside Southern polities?',
    'Incidence analysis of tariff revenue and subsidy spending, paired with comparative sectoral studies of infant-industry outcomes under protection.',
    'If captured, Southern households become covert net payers and the beneficiary structure splits internally; the accommodation function shrinks toward protection for the connected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_space_capture, empirical, 'Internal distribution of who actually captures the accommodated space.').

omega_variable(
    pta_exit_substitution,
    'Do preferential trade agreements constitute functioning exit from the multilateral bargain?',
    'Compare preferential-agreement chapter depth against WTO disciplines (especially IP chapters exceeding TRIPS) and model welfare under hub-and-spoke versus multilateral counterfactuals.',
    'If exit is real, suppression is overstated and the coercive grip weakens; if preferential tracks replicate or deepen the same disciplines, exit is illusory and suppression understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pta_exit_substitution, empirical, 'Whether the preferential-agreement network provides genuine alternatives to the multilateral arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_treaty_framework__developmental_reading, theater_ratio, 0, 0.26).
narrative_ontology:measurement_basis(wto__tr_t0, observed).
narrative_ontology:measurement(wto__tr_t5, wto_treaty_framework__developmental_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(wto__tr_t5, observed).
narrative_ontology:measurement(wto__tr_t10, wto_treaty_framework__developmental_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(wto__tr_t10, observed).
narrative_ontology:measurement(wto__tr_t15, wto_treaty_framework__developmental_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(wto__tr_t15, observed).
narrative_ontology:measurement(wto__tr_t20, wto_treaty_framework__developmental_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(wto__tr_t20, observed).
narrative_ontology:measurement(wto__tr_t25, wto_treaty_framework__developmental_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement_basis(wto__tr_t25, observed).
narrative_ontology:measurement(wto__tr_t30, wto_treaty_framework__developmental_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(wto__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_treaty_framework__developmental_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(wto__be_t0, observed).
narrative_ontology:measurement(wto__be_t5, wto_treaty_framework__developmental_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(wto__be_t5, observed).
narrative_ontology:measurement(wto__be_t10, wto_treaty_framework__developmental_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(wto__be_t10, observed).
narrative_ontology:measurement(wto__be_t15, wto_treaty_framework__developmental_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement_basis(wto__be_t15, observed).
narrative_ontology:measurement(wto__be_t20, wto_treaty_framework__developmental_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(wto__be_t20, observed).
narrative_ontology:measurement(wto__be_t25, wto_treaty_framework__developmental_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement_basis(wto__be_t25, observed).
narrative_ontology:measurement(wto__be_t30, wto_treaty_framework__developmental_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(wto__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_treaty_framework__developmental_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(wto__su_t0, observed).
narrative_ontology:measurement(wto__su_t5, wto_treaty_framework__developmental_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement_basis(wto__su_t5, observed).
narrative_ontology:measurement(wto__su_t10, wto_treaty_framework__developmental_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(wto__su_t10, observed).
narrative_ontology:measurement(wto__su_t15, wto_treaty_framework__developmental_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(wto__su_t15, observed).
narrative_ontology:measurement(wto__su_t20, wto_treaty_framework__developmental_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(wto__su_t20, observed).
narrative_ontology:measurement(wto__su_t25, wto_treaty_framework__developmental_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(wto__su_t25, observed).
narrative_ontology:measurement(wto__su_t30, wto_treaty_framework__developmental_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(wto__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__developmental_reading, resource_allocation).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the WTO treaty framework' decomposes into two structurally distinct readings of one fixed-text kernel, authored as separate stories per the epsilon-invariance principle. This file carries the developmental reading: permanent structural accommodation, transfer as core commitment, Southern states as primary beneficiaries, moderate epsilon concentrated on Northern IP and exporter interests. The sibling file carries the market_access reading: symmetric universal obligation, transitional exceptions, extraction concentrated on Southern policy autonomy. Same text, different epsilon, different beneficiary structure, different type. Neither reading is strictly upstream of the other: each cites treaty-lineage evidence against the other, and ministerial practice feeds both — the link here records the family relation and routes contamination-propagation analysis between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
