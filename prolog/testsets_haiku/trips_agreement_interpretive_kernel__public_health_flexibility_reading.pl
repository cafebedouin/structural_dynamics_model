% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Reading: Compulsory Licensing and Parallel Imports
 *   domain: international_trade/intellectual_property/public_health
 *
 * SUMMARY:
 *   The TRIPS agreement (1995) contains two interpretively opposed kernels—a
 *   fixed text that can be read to prioritize either patent protection or
 *   public health access. This story instantiates the PUBLIC HEALTH
 *   FLEXIBILITY READING: a legal-technical interpretation of TRIPS Articles
 *   31 (compulsory licensing), 31bis (public health-driven licensing), and 6
 *   (parallel imports exhaustion) that treats these provisions as substantive
 *   flexibilities permitting member states to override patent monopolies when
 *   public health emergencies justify the override. Under this reading,
 *   generic manufacturers and health ministries become beneficiaries with
 *   expanded legal authority; pharmaceutical patent holders face pricing
 *   pressure and market-exclusivity erosion. The strong exclusivity reading
 *   (separate constraint story) interprets the same TRIPS text as protecting
 *   high-level patent rights with only narrow, government-use
 *   exceptions—treating public health flexibilities as marginal or requiring
 *   onerous notice to patent holders. Both readings are live positions held
 *   by different WTO member-state coalitions; neither logically forecloses
 *   the other within the TRIPS framework itself—the text admits both
 *   readings. The dispute between them is settled operationally by WTO
 *   dispute panels, which have endorsed the flexibility reading in major
 *   cases (India generics disputes, COVID vaccine access disputes). The
 *   present story models the flexibility reading as a TANGLED ROPE: it
 *   coordinates legitimate public health access with innovation incentives
 *   through a common framework, but does so asymmetrically—generic
 *   manufacturers and health ministries gain negotiating leverage and
 *   production authority where patent holders lose monopoly rents and pricing
 *   control. Active enforcement is required to hold this reading against the
 *   exclusivity-reading interpretation, which would reverse the
 *   beneficiary/victim assignments.
 *
 * KEY AGENTS:
 *   - Generic pharmaceutical manufacturers: organized actors gaining production authority under compulsory licensing and parallel import provisions
 *   - Health ministries: institutional agenda-setters with authority to invoke compulsory licensing during crises; structurally constrained by budget and political dependency on trade relationships
 *   - Patent-holding pharmaceutical firms: powerful payers facing market-exclusivity erosion and pricing pressure; defend the strong exclusivity reading through trade disputes and lobbying
 *   - WTO dispute settlement panels: institutional agenda-setters holding binding interpretive authority over TRIPS text; panels have endorsed the flexibility reading in major precedents
 *   - Disease-affected populations: powerless beneficiaries with no negotiating voice in TRIPS disputes; trapped in their health status and income level; ultimate referent of the 'public health' justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.42).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Reading: Compulsory Licensing and Parallel Imports").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade/intellectual_property/public_health").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'd092c993-bcba-42ef-9c11-58fe39668782').
narrative_ontology:cs_kernel_codification('d092c993-bcba-42ef-9c11-58fe39668782', formalized).
narrative_ontology:cs_authority_grounding('d092c993-bcba-42ef-9c11-58fe39668782', lineage).
narrative_ontology:cs_interpretation_layer_present('d092c993-bcba-42ef-9c11-58fe39668782').
narrative_ontology:cs_reading_relation('d092c993-bcba-42ef-9c11-58fe39668782', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('d092c993-bcba-42ef-9c11-58fe39668782', foundational, public_health_primacy_in_crisis).
narrative_ontology:cs_axiom_status(public_health_primacy_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('d092c993-bcba-42ef-9c11-58fe39668782', public_health_primacy_in_crisis, deontological).
narrative_ontology:cs_axiom('d092c993-bcba-42ef-9c11-58fe39668782', foundational, compulsory_licensing_broad_scope_authorized).
narrative_ontology:cs_axiom_status(compulsory_licensing_broad_scope_authorized, holdable).
narrative_ontology:cs_axiom_grounding('d092c993-bcba-42ef-9c11-58fe39668782', compulsory_licensing_broad_scope_authorized, conventional).
narrative_ontology:cs_reference_frame('d092c993-bcba-42ef-9c11-58fe39668782', trips_articles_31_and_6_public_health_exception).
narrative_ontology:cs_drift_state('d092c993-bcba-42ef-9c11-58fe39668782', post_covid_vaccine_access_era_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d092c993-bcba-42ef-9c11-58fe39668782', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, public_health_advocates).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patent_holding_pharmaceutical_firms).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, research_intensive_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, disease_affected_populations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_member_states_high_income).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_article_31_compulsory_licensing_authority).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_article_6_parallel_import_legitimacy).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, right_to_health_primacy_over_ip).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain expanded production rights through compulsory licensing (Articles 31, 31bis) and parallel import authority (Article 6). Under this reading, they can manufacture patented medicines for domestic consumption and export to countries with public health emergencies. Their market access expands where patent exclusivity would otherwise prevent entry. They operate in jurisdictions with weaker enforcement capacity and coordinate through generic industry associations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Can invoke compulsory licensing to authorize domestic production of patented medicines during health crises (epidemics, pandemics). Can import generic versions from countries where parallel imports are legal, bypassing patent restrictions. Set health procurement policy around the flexibility reading. Operate under budget constraints and populations with limited ability to pay patent-protected prices.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, agenda_setter).

% Face erosion of market exclusivity and pricing power under broad compulsory licensing construction. Markets they previously controlled via patent exclusivity become accessible to generics. Revenue from high-income jurisdictions remains protected, but middle-income and low-income markets open to competition. Patent term certainty and the assumption of 20-year monopoly pricing are reduced where health ministries invoke the flexibility reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patent_holding_pharmaceutical_firms, payer,
    powerful, biographical, constrained, global).

% Experience reduced incentive structure under a broad flexibility reading: investment in breakthrough therapies depends on global patent protection and premium pricing in multiple markets. If compulsory licensing and parallel imports become normalized early in a drug's life cycle, the return-on-investment window compresses. They argue future innovation will decline if extraction of monopoly rents is systematically constrained.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, research_intensive_innovators, payer,
    powerful, generational, constrained, global).

% Holds interpretive authority over TRIPS text through dispute panels. A panel's reading of Article 31 (compulsory licensing scope) and Article 6 (parallel import legality) carries enforcement through trade retaliation mechanisms. Under this reading, panels have endorsed broad public health flexibilities. Their authority to settle the interpretation is itself contested—the strong exclusivity reading disputes whether panels should read TRIPS flexibilities expansively.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement, agenda_setter,
    institutional, generational, analytical, global).

% Represent pharmaceutical patent holders at the negotiating table and in trade disputes. Push back against broad flexibility readings through dispute initiation and market-access negotiations. Face domestic pharmaceutical lobbies demanding patent enforcement. Constrained by TRIPS commitments they signed but retain some leverage through dispute process.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_member_states_high_income, payer,
    institutional, generational, constrained, global).

% Benefit from lower generic drug prices and expanded access under compulsory licensing and parallel imports. Cannot afford patent-protected prices in crisis situations (HIV/AIDS, tuberculosis, malaria, COVID-19 vaccines). Have no negotiating power in TRIPS treaty disputes but are the ultimate beneficiaries of the flexibility reading. Trapped in their health status and income level; must rely on health ministries to invoke flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, disease_affected_populations, beneficiary,
    powerless, immediate, trapped, national).

% Analyze and interpret TRIPS text from a legal/technical standpoint. Do not have formal authority to settle disputes but produce legal advice and precedent analysis. Observe the contest between readings and the enforcement mechanisms each reading produces.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_secretariat_and_legal_experts, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__public_health_flexibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances two legitimate global interests: intellectual property incentives for pharmaceutical innovation and equitable access to essential medicines during public health emergencies. Establishes a common framework (TRIPS) that most WTO members commit to, with built-in flexibility mechanisms (compulsory licensing, parallel imports) to permit exceptions when public health justifies them. Solves the collective-action problem of unilateral defection from patent protection—a single country breaking ranks would face trade retaliation unless the break is framed as authorized under TRIPS itself.
% TRANSFER_FUNCTION: Moves intellectual property monopoly rights from health ministries and generic manufacturers to patent holders. Under the flexibility reading, this transfer is conditional and subject to compulsory licensing override where public health requires it. Generic manufacturers transfer knowledge/capability into authorized production capacity. Pharmaceutical firms transfer temporary market exclusivity into premium pricing power in protected markets. The flexibility reading creates a two-tier system: stronger exclusivity in high-income markets, broader generic access in lower-income or crisis-affected jurisdictions.
% ABSENT_VOICES: Patients in middle-income countries without compulsory licensing frameworks in place are excluded from the conversation—they cannot advocate for themselves in WTO dispute panels or trade negotiations. Generic manufacturers in countries without domestic production capacity lack institutional voice. Small countries with limited health systems have minimal negotiating power in dispute settlement. Treatment-resistant tuberculosis patients, HIV-positive populations in sub-Saharan Africa, and other disease-affected groups are structurally absent despite being the ultimate referents of the 'public health' justification.
% DISAPPEARANCE_RATIONALE: If this reading vanished and only the strong exclusivity reading held, patent holders would enforce full 20-year monopolies globally; generic production and parallel imports would be treated as patent infringement; health ministries would lose legal authorization for compulsory licensing except in explicitly narrower circumstances (only for government non-commercial use under Article 31(b) without the broad public health interpretation). Medicine prices would rise in middle-income and low-income countries; some populations would lose treatment access; compulsory licensing litigation would intensify; vaccine and drug supply chains would reorganize around patent-protected production. The global pharmaceutical market, drug discovery investment levels, and health outcomes would all shift—the reading is not marginal to the arrangement.
% FOUNDING_PROBLEM: TRIPS was negotiated (1994) in a context where member states recognized that uniform, high-level patent protection could collide with legitimate public health crises (epidemics, pandemics, endemic diseases in poor countries). The text deliberately embedded flexibilities (compulsory licensing, parallel imports, exhaustion of rights) to permit exceptions when health emergencies justify them. The founding problem is the need to protect innovation incentives while preserving state capacity to override patent monopolies when populations face death or untreated disease.
% FOUNDING_PROBLEM_CORROBORATION: WHO and health advocacy organizations affirm the founding problem is live: COVID-19 vaccine access disputes (2020–2022) showed that patent protection prevents rapid scaling of vaccine production in countries with manufacturing capacity, and that compulsory licensing is not automatically invoked because pharmaceutical firms and high-income country governments dispute its scope. The strong exclusivity reading counters that innovation-incentive protection IS the founding problem, not health access—arguing that without strong patent protection, pharmaceutical R&D investment will decline and future pandemics will lack effective treatments. Dispute panel testimony from developing-country governments and WTO technical analysis support the public health reading; developed-country governments' actions in COVID vaccine negotiations support the exclusivity reading. No consensus exists among parties outside the medical and legal professions.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).
:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.38 at interval end, declining from 0.52 at TRIPS inception in 1995) reflects the public health reading's core claim: broad flexibilities substantially reduce the monopoly extraction patent holders can conduct. Generic manufacturers can enter markets that would otherwise be locked by patent exclusivity; health ministries can authorize production without negotiating licensing fees with patent holders. However, extractiveness does not fall to near-zero because the flexibility reading still operates WITHIN a patent system that grants significant protection in high-income markets and for initial market entry. Suppression (0.42) models the enforcement cost: the flexibility reading must be actively defended against the strong exclusivity reading through dispute settlement, national legislation embodying the flexibility interpretation, and negotiation of compulsory licensing terms. Theater ratio (0.28, rising modestly from 0.15) tracks the performative layer: public health justifications are genuine (disease access is the real coordination problem), but an increasing share of disputation energy goes to defending the reading itself against exclusivity-framing challenges, especially during COVID-19 vaccine access crises (2020–2022) where theater rose to 0.30 as the reading was invoked but not consistently operationalized. The measurement series tracks one shared time grid across all three metrics to avoid misalignment (OQ-105 precaution): every metric is authored at 1995, 2001, 2008, 2015, 2020, 2025. Extractiveness shows secular decline as the reading becomes more established in dispute precedent and more states incorporate it into domestic law; suppression stays moderate because the exclusivity reading remains a live, powerful counter-position held by high-income countries and pharmaceutical firms; theater reflects the gap between invocation (TRIPS Article 31 text exists, panels cite it) and operationalization (many countries still lack domestic compulsory licensing frameworks; COVID-19 showed the reading was cited but not uniformly acted upon).
 *
 * PERSPECTIVAL GAP:
 *   From a generic manufacturer's or health ministry's seat, the flexibility reading is a legitimate, even minimal protection of state sovereignty against monopoly capture—it reads TRIPS as preserving emergency-override authority. From a pharmaceutical firm's or research-intensive innovator's seat, the same reading is an economically destructive expansion of compulsory licensing authority that erodes the return-on-investment assumptions they built their R&D programs around. From a WTO dispute panel's seat (institutional observer), the reading is a legal-technical interpretation constrained by the text's actual language, the subsequent agreements and understandings (Doha Declaration, TRIPS Council guidance), and precedent. The engine computes per-seat directionality from this structural asymmetry: health ministries and generics get low d (high benefits, reduced costs), patent firms get high d (reduced benefits, increased costs), disease-affected populations get highest d (they are the referent but have no exit options or negotiating power). The gap between payer and beneficiary seats is enormous and structurally unavoidable—the reading necessarily advantages one seat by disadvantaging another.
 *
 * DIRECTIONALITY LOGIC:
 *   Generic manufacturers (organized, mobile) benefit from expanded production rights under compulsory licensing and parallel imports; their directionality is low (d ≈ 0.25) because they have exit options (they can operate in jurisdictions with strong exclusive-patent readings if they adapt their supply chains). Health ministries (institutional, constrained exit) also benefit but are heavily dependent on the flexibility reading holding against challenge; their directionality is moderate-low (d ≈ 0.35) because they are structurally constrained—they represent populations and operate under budget constraints that make the flexibility reading politically necessary, but they cannot exit TRIPS itself or the global trade system. Patent-holding pharmaceutical firms (powerful, globally integrated, constrained by the reading to specific jurisdictions) face costs in form of lost exclusivity and pricing power; their directionality is high (d ≈ 0.75) because they are the primary targets of the reading's extraction: market-exclusivity erosion IS the mechanism through which the reading operates. Research-intensive innovators face similar directionality (d ≈ 0.70) because the reading reduces their ability to recover R&D investment through monopoly pricing. Disease-affected populations (powerless, trapped) are structural targets in a different sense—they have no exit and cannot negotiate the terms of their own access; their directionality approaches (d ≈ 0.95) because they are fully dependent and have no alternative. WTO dispute panels do not have a directionality on this constraint because they are observers of the interpretive contest, not parties to the distribution; they compute the reading's validity but do not benefit or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing innovation incentives with public health access) remains contested, not dead—COVID-19 vaccine access disputes (2020–2022) proved the problem is live and unresolved. However, MANDATROPHY WARNING is present: the flexibility reading is invoked rhetorically much more often than it is operationalized. Many countries cite TRIPS Article 31 but lack domestic compulsory licensing frameworks; the reading is used as a negotiating threat more than an implemented tool. During COVID-19, the reading could have been invoked for vaccine manufacturing scale-up but was mostly not—instead, high-income countries hoarded doses and the reading remained dormant. This is evidence of THEATER DRIFT: the founding problem is treated as solved (we cite the flexibility! we have the dispute precedent!) even as the underlying coordination function (equitable pandemic response) failed. The constraint shows signs of becoming PITON: the flexibility reading is maintained ceremonially (panels cite it, WHO references it, developing countries invoke it in negotiations) but actual material redistribution through compulsory licensing remains rare. The rise in theater_ratio from 0.15 (1995) to 0.30 (2020) and decline to 0.28 (2025) reflects this pattern: maximum theater during COVID-19 as the reading was most invoked and least operationalized, slight retreat afterward as the ceremony is maintained but with less urgency. Mandatrophy is NOT YET RESOLVED because the founding problem remains live (disease access crises continue, innovation needs persist), but the constraint shows warning signs of becoming a performed commitment rather than a functioning one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compulsory_licensing_scope_ambiguity,
    'Does TRIPS Article 31 permit compulsory licensing for general public health emergencies (epidemics, endemic disease, vaccine access crises) or only for government non-commercial use and patent-holder-unremunerated situations as described in Article 31(b)?',
    'WTO dispute panel interpretation of Article 31 in the context of Article 31bis (Doha waiver) and the TRIPS Council''s General Council decision on public health. Textual analysis of what ''public health emergency'' means within Article 31bis (2003). Subsequent state practice: which countries invoke Article 31 for public health and how panels respond.',
    'If Article 31 is read broadly, compulsory licensing becomes a normal tool for balancing access and innovation; generic manufacturers and health ministries are structurally empowered; extractiveness remains moderate and the flexibility reading holds. If read narrowly, compulsory licensing requires onerous administrative processes and patent-holder negotiation; pharmaceutical firms retain substantial pricing power; the reading shifts toward the strong exclusivity framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_licensing_scope_ambiguity, conceptual, 'Whether public health scope of compulsory licensing is broad or narrow—a reading question.').

omega_variable(
    parallel_import_exhaustion_interpretation,
    'Does TRIPS Article 6 (exhaustion of rights) permit unrestricted parallel imports once a patented good is sold in any country, or does it permit countries to adopt regional exhaustion regimes that restrict parallel imports?',
    'WTO dispute panel rulings on Article 6. State practice in regional trade agreements (CPTPP, RCEP, African Union agreements) and their interpretation of exhaustion. Textual analysis of what ''nothing in this Agreement shall be construed...'' means in Article 6.',
    'Broad interpretation of Article 6 (international exhaustion) expands parallel import authority and generic-manufacturer market access across jurisdictions; strengthens the flexibility reading. Narrow interpretation (national exhaustion allowed) permits countries to restrict parallel imports and maintain higher prices; favors the strong exclusivity reading. This affects whether generic medicines manufactured under compulsory license can be exported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parallel_import_exhaustion_interpretation, conceptual, 'Whether Article 6 permits national exhaustion regimes or requires international exhaustion—affects parallel import scope.').

omega_variable(
    dispute_settlement_interpretive_authority,
    'Who holds ultimate interpretive authority over TRIPS text: WTO dispute panels (with enforcement through trade retaliation), sovereign member states (through national legislation and municipal courts), or the TRIPS Council (through consensus guidance)?',
    'WTO Appellate Body and dispute panel precedent. State behavior when panels rule against their preferred reading (compliance vs. defiance). Evolution of the TRIPS Council''s role in issuing general guidance (Doha Declaration, waiver decisions, Council guidance notes).',
    'If panels hold supreme authority, the flexibility reading is locked in by precedent and member states must operationalize it domestically or face dispute challenges. If member states retain ultimate authority through municipal law, states can adopt exclusivity-favoring domestic legislation that contradicts panel precedent. If TRIPS Council consensus is supreme, the reading is subject to renegotiation and member-state blocking power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dispute_settlement_interpretive_authority, conceptual, 'Locus of TRIPS interpretive authority and its enforcement mechanism—determines whether this reading is operationally binding or subject to defection.').

omega_variable(
    innovation_incentive_empirical_claim,
    'What is the empirical relationship between pharmaceutical R&D investment and patent protection levels? Does restricting patent monopolies through broad compulsory licensing actually reduce future drug discovery investment, or is that a counterfactual assertion?',
    'Econometric analysis of R&D investment trends in countries with strong vs. weak compulsory licensing frameworks. Comparison of drug discovery output (new chemical entities, breakthrough therapies) before and after countries adopted flexibilities. Pharmaceutical firm testimony on internal R&D allocation decisions.',
    'If strong empirical evidence shows compulsory licensing reduces R&D investment, the exclusivity reading gains force: the flexibility reading''s extraction of monopoly rents undermines innovation incentives. If evidence shows R&D is robust to compulsory licensing (innovation driven by other factors: market size, tax incentives, IP protection for process improvements), the flexibility reading is insulated from the innovation-disincentive critique.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_incentive_empirical_claim, empirical, 'Causal link between compulsory licensing scope and pharmaceutical R&D investment—contested empirical premise underlying the strong exclusivity reading''s victim justification.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (balancing innovation incentives with public health access) been solved by alternative mechanisms—compulsory licensing frameworks are now mature, generic industry is established, WHO supply chains are resilient, developing countries have domestic production capacity—such that TRIPS flexibilities are no longer structurally necessary?',
    'Comparative analysis of public health outcomes pre/post-TRIPS, controlling for income and disease burden. Assessment of whether countries without compulsory licensing frameworks (those favoring strong exclusivity reading) suffer worse health outcomes or higher prices. Evidence from COVID-19 vaccine access: did flexibility reading enable rapid access, or did other factors (bilateral purchase, technology transfer, development aid) matter more?',
    'If the founding problem is solved, TRIPS flexibilities are ceremonial and the constraint is PITON. Both readings remain live but the underlying coordination function has atrophied. If the founding problem remains live (disease access crises recur, prices remain unaffordable in low-income settings), the flexibility reading is operationally necessary and not ceremonial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether public health access problems persist such that TRIPS flexibility mechanisms remain functionally necessary or have become ceremonial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(trip_tr_t1995, observed).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement_basis(trip_tr_t2001, observed).
narrative_ontology:measurement(trip_tr_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement_basis(trip_tr_t2008, observed).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(trip_tr_t2015, observed).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement_basis(trip_tr_t2020, observed).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(trip_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement_basis(trip_be_t1995, observed).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.45).
narrative_ontology:measurement_basis(trip_be_t2001, observed).
narrative_ontology:measurement(trip_be_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement_basis(trip_be_t2008, observed).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement_basis(trip_be_t2015, observed).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement_basis(trip_be_t2020, observed).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(trip_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement_basis(trip_su_t1995, observed).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.38).
narrative_ontology:measurement_basis(trip_su_t2001, observed).
narrative_ontology:measurement(trip_su_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement_basis(trip_su_t2008, observed).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement_basis(trip_su_t2015, observed).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement_basis(trip_su_t2020, observed).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(trip_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.18).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_pharmaceutical_patent_dispute_settlement).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_drug_market_access_regime).

% DUAL FORMULATION NOTE:
% This story is one reading of a contested kernel (TRIPS interpretive kernel, trips_agreement_interpretive_kernel). The same text (TRIPS Articles 1–73, particularly Articles 31, 31bis, 6) is read to permit either broad public health flexibilities (this story: public_health_flexibility_reading) or narrow patent protection with minimal exceptions (sibling story: strong_exclusivity_reading). Both readings are live positions held by different WTO member coalitions. The dispute between them is settled operationally by WTO dispute settlement panels, which have endorsed the flexibility reading. The network link to dispute_settlement_interpretive_authority captures the fact that this reading's operationalization depends on which seat holds ultimate interpretive authority over TRIPS.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
