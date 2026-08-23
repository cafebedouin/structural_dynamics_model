% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: Territorial Sovereignty Bound on Regulatory Jurisdiction (GDPR Article 3 Scope — Territorial Reading)
 *   domain: technology governance/international law/privacy regulation
 *
 * SUMMARY:
 *   This story instantiates the territorial_sovereignty_reading of the kernel
 *   gdpr_article_3_scope: the claim that legitimate regulatory authority is
 *   bounded by territorial sovereignty, and that extraterritorial application
 *   of data-protection law (Article 3(2)-style targeting and monitoring reach
 *   into non-EU controllers) exceeds it. The constraint under classification
 *   is the territorial bounding arrangement itself as it operatively
 *   maintains jurisdictional limits in the data-governance domain —
 *   non-recognition of foreign regulatory claims, blocking statutes, court
 *   refusal to enforce foreign orders, localization defenses — against the
 *   sustained pressure of the extraterritorial GDPR practice. Per the
 *   epsilon-referent rule, extractiveness is authored for THIS standing
 *   arrangement, assessed by the reading's own lights: the reading regards
 *   the bound as broadly legitimate (hence moderate-low epsilon), while the
 *   descriptively-authored metrics record its real costs — the cross-border
 *   protection gap it tolerates, the enforcement reach it removes from EU
 *   regulators, and the enforcement machinery needed to hold the line. The
 *   claimed type and the metrics are independently authored; the engine
 *   computes per-seat classifications from the structural data. Sibling
 *   readings (effects_jurisdiction_reading, market_access_reading) are
 *   separate constraints with their own files; they are referenced only
 *   through network links, omegas, and cs_structure.reading_relations, never
 *   folded into this classification.
 *
 * KEY AGENTS:
 *   - - non_eu_sovereign_regulators: agenda_setter ([institutional]/[arbitrage]) — upholds and administers jurisdictional limits through non-recognition, blocking legislation, and domestic-court refusal to enforce foreign regulatory orders; can shift between defensive invocation and offensive extraterritorial projection of their own
 *   - - non_eu_digital_firms: beneficiary ([powerful]/[arbitrage]) — shielded from foreign regulatory obligation in home jurisdictions and able to route data, incorporate, and structure operations to exploit jurisdictional seams
 *   - - domestic_intelligence_agencies: beneficiary ([institutional]/[arbitrage]) — their domestic data practices are insulated from external audit or foreign legal process by the same wall that shields commercial regulation
 *   - - transnational_data_subjects: payer ([powerless]/[trapped]) — individuals whose data circulates across borders; when harmed by a processor outside their jurisdiction, redress is blocked by the very boundary the arrangement maintains; no exit from their data's circulation exists
 *   - - small_open_jurisdictions: payer, secondarily beneficiary ([moderate]/[constrained]) — invoke the bound defensively like every state, but lack the market leverage that makes the bound effective; they absorb both encroachment by stronger powers and the fragmentation costs of incompatible regimes
 *   - - eu_data_protection_authorities: payer ([institutional]/[constrained]) — seek to extend protection to residents' data wherever processed; the bound caps that reach, forcing reliance on adequacy diplomacy and targeted enforcement against entities deliberately serving EU users
 *   - - cross_border_privacy_ngo_coalition: excluded ([organized]/[constrained]) — advocates protection-following-the-person framings and litigates for cross-border redress; sits outside the state-to-state conversation in which jurisdictional norms are negotiated
 *   - - comparative_jurisprudence_scholars: observer ([analytical]/[analytical]) — map the doctrinal contest between territorial, effects, and market-access accounts; collect evidence no seat inside the contest gathers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.3).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.55).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "Territorial Sovereignty Bound on Regulatory Jurisdiction (GDPR Article 3 Scope — Territorial Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology governance/international law/privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'b7126b72-a595-4b4f-bd72-3d3dd57701f6').
narrative_ontology:cs_kernel_codification('b7126b72-a595-4b4f-bd72-3d3dd57701f6', fixed_text).
narrative_ontology:cs_authority_grounding('b7126b72-a595-4b4f-bd72-3d3dd57701f6', lineage).
narrative_ontology:cs_interpretation_layer_present('b7126b72-a595-4b4f-bd72-3d3dd57701f6').
narrative_ontology:cs_reading_relation('b7126b72-a595-4b4f-bd72-3d3dd57701f6', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7126b72-a595-4b4f-bd72-3d3dd57701f6', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('b7126b72-a595-4b4f-bd72-3d3dd57701f6', foundational, regulatory_legitimacy_requires_territorial_basis).
narrative_ontology:cs_axiom_status(regulatory_legitimacy_requires_territorial_basis, holdable).
narrative_ontology:cs_axiom_grounding('b7126b72-a595-4b4f-bd72-3d3dd57701f6', regulatory_legitimacy_requires_territorial_basis, conventional).
narrative_ontology:cs_axiom('b7126b72-a595-4b4f-bd72-3d3dd57701f6', foundational, obligations_cannot_be_imposed_without_political_membership).
narrative_ontology:cs_axiom_status(obligations_cannot_be_imposed_without_political_membership, holdable).
narrative_ontology:cs_axiom_grounding('b7126b72-a595-4b4f-bd72-3d3dd57701f6', obligations_cannot_be_imposed_without_political_membership, deontological).
narrative_ontology:cs_reference_frame('b7126b72-a595-4b4f-bd72-3d3dd57701f6', westphalian_territorial_exclusivity).
narrative_ontology:cs_drift_state('b7126b72-a595-4b4f-bd72-3d3dd57701f6', post_gdpr_extraterritorial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7126b72-a595-4b4f-bd72-3d3dd57701f6', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_sovereign_regulators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_digital_firms).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, domestic_intelligence_agencies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, transnational_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, small_open_jurisdictions).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, small_open_jurisdictions).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, territorial_exclusivity_principle).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, consent_based_obligation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States and their regulators outside the EU maintain that their authority runs to their borders and no farther. They enact blocking statutes, decline to enforce foreign regulatory orders in domestic courts, and treat foreign claims over domestically-held data as encroachments to be resisted. The same actors reserve the option of projecting their own rules outward when advantage favors it. What flows to them is preserved exclusive control of domestic regulatory space; what they forgo is participation in any unified transnational scheme. Exit from the arrangement would mean accepting foreign regulatory authority inside their territory, which no state offers.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_sovereign_regulators, agenda_setter,
    institutional, generational, arbitrage, global).

% Multinationals headquartered outside the EU face foreign data-protection obligations only where they deliberately serve that market. Inside their home jurisdictions they operate free of externally imposed privacy regimes, and they structure data storage, incorporation, and service geography to keep obligations voluntary and priced. Compliance with any single regime is treated as a market-entry cost chosen where revenue justifies it. Their exit options are unusually rich: jurisdictional seams are exploitable at low cost through routing and corporate structure.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_digital_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Security and intelligence services of sovereign states conduct domestic data collection under national law. The jurisdictional wall insulates those programs from foreign legal process, external audit, and transnational oversight bodies. What flows to them is operational freedom inside their own borders; what they cede is any claim to reach analogous programs abroad except through their own state's power. Their position depends entirely on which state they serve.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, domestic_intelligence_agencies, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals whose personal data is collected, transferred, and processed across multiple jurisdictions. When a processor outside their home jurisdiction harms them, they confront a wall: their domestic regulator lacks authority there, the foreign regulator owes them nothing, and cross-border litigation is costly and uncertain. They cannot withdraw their data from circulation, relocate their digital lives meaningfully, or opt into a stronger regime. Every protection they hold stops functioning at the border.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, transnational_data_subjects, payer,
    powerless, biographical, trapped, global).

% Small economies dependent on trade, finance, and data services invoke jurisdictional limits like every state, and rhetorically the norm protects them. In practice the norm's protection scales with the power to enforce it: larger states' legal processes reach into their banks, clouds, and platforms regardless, while their own reciprocal reach goes nowhere. They absorb the costs of incompatible neighboring regimes without collecting the autonomy dividend the arrangement promises its stronger members. Leaving the arrangement is not available; their option space is choosing which larger bloc's rules to accommodate.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, small_open_jurisdictions, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, small_open_jurisdictions, beneficiary).

% European supervisory authorities hold statutory duties toward residents whose data is processed anywhere. The jurisdictional limit caps how far they may pursue those duties: enforcement against controllers outside the territory is confined to entities that deliberately target or monitor residents, and even there it depends on cooperation they cannot compel. They respond with adequacy diplomacy, standard contractual clauses, and selective action against high-profile targets. Abandoning the duties is not an option their mandate permits; their flexibility is in method, not objective.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, payer,
    institutional, generational, constrained, continental).

% Civil-society organizations specializing in privacy and digital rights advocate protection models that follow the person rather than the border, and bring strategic litigation for cross-border redress. They are not participants in the intergovernmental settings where jurisdictional norms are actually negotiated, which proceed state-to-state. Their influence runs through courts and public opinion; their exclusion from the bargaining table is durable because states negotiate as states.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, cross_border_privacy_ngo_coalition, excluded,
    organized, biographical, constrained, global).

% Academic specialists in transnational law and data governance document how jurisdictional claims are made, resisted, and compromised across the major regimes. They publish mappings of the territorial, effects, and market-access accounts and track which instruments quietly blend them. They collect and attest evidence that no seat inside the contest has incentive to compile, and they bear no stake in which account prevails.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, comparative_jurisprudence_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_sovereign_regulators).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__territorial_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains reciprocal respect for territorial regulatory boundaries among states: each forgoes projecting regulatory authority into the others' territories in exchange for immunity from the others' projections, preventing duplicative, conflicting, and mutually escalating jurisdictional claims over shared digital infrastructure.
% TRANSFER_FUNCTION: Moves regulatory discretion and enforcement immunity across borders: states and firms inside protected territories receive freedom from foreign regulatory obligation; the corresponding burdens — unredressed cross-border harm, curtailed protective enforcement, navigational complexity — move to transnational data subjects, to regulators seeking reach beyond their borders, and to multinationals managing fragmented regimes.
% ABSENT_VOICES: Transnational data subjects and the civil-society organizations representing them are absent from the state-to-state settings where jurisdictional norms are negotiated; treaty-based harmonization bodies are structurally sidelined. Present, they would argue for protection-following-the-person or conditional-market-access framings that dissolve the territorial wall, and would contest the treatment of unrepresented foreign data subjects as outside anyone's obligation.
% DISAPPEARANCE_RATIONALE: If the bound vanished overnight, every regulator with a statutory duty would claim global reach simultaneously, retaliation and blocking would escalate within months, platforms would geoblock or fragment services by region, and emergency treaty negotiations would begin immediately. Current jurisdictional stability is substantially the bound holding under continuous pressure; its absence is visible instantly.
% FOUNDING_PROBLEM: Preventing interstate overreach across borders: the Westphalian settlement was built to stop armies, tariffs, and edicts from crossing into sovereign territory, and the digital-era extension of the same problem is stopping any single bloc's regulatory order from silently absorbing governance of data flows that cross every border.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any single benefiting party: EU institutions invoke territorial limits themselves when targeted (their objections to foreign statutes reaching data held in Europe mirror the bound they resist when they are the projector); international-law scholarship spanning both camps attests the live sovereignty problem; developing-country negotiating positions in trade fora attest it from the weak-power seat. Corroboration is nonetheless positional — the same great powers attesting the norm violate it instrumentally — which the selective_invocation_asymmetry omega tracks explicitly.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).
:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.30 is authored from this reading's own lights: the bound's core operation (reciprocal non-interference) is coordination the reading regards as legitimate, so epsilon sits well below extraction-dominated profiles — but not near zero, because the arrangement's costs are real and borne by identifiable parties: the cross-border redress gap widens as data flows globalize (series rises 0.12 to 0.30 across the interval, tracking the growth in volume and value of cross-border personal data). Suppression 0.55 records the arrangement's active discouragement of transnational regulatory projects: blocking statutes, non-recognition doctrine, and localization defenses do not merely decline participation, they raise the cost of alternatives. Accessibility_collapse 0.40: workable alternatives survive — adequacy determinations, bilateral frameworks, treaty accession, sectoral agreements — so understanding the bound does not foreclose substitutes. Resistance 0.62 is high and structural: Article 3(2) practice, the effects-jurisdiction scholarly coalition, and NGO litigation continuously press against the bound. Theater_ratio 0.18: most boundary maintenance is functional (real blocking statutes, real non-enforcement rulings), with a growing minority of purely symbolic sovereignty declarations. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: a ratchet upward through the Safe Harbor collapse and localization-wave years (t=20), peaking around the GDPR/CLOUD Act/Schrems II collision (t=23-26), then partially relaxing as adequacy and the Data Privacy Framework re-opened accommodation channels (t=30) — an enforcement-hardening-then-partial-accommodation arc, not a static picture, which is exactly the case the temporal series exists for. All three series share one eight-point grid; no metric borrows another's timeline.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the sovereign-regulator seat the arrangement presents as ordinary reciprocal coordination each state would demand even in isolation. From the transnational-data-subject seat the same wall is experienced as the place where harm goes unredressed — a trap with no exit, the highest-chi experience in the structure despite the lowest power. From the EU-regulator seat it is a gag on a protective duty the seat considers obligatory. From the multinational-firm seat it is a seam to arbitrage, and therefore cheap. The engine derives these divergences from role, power, and exit data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for non_eu_sovereign_regulators, non_eu_digital_firms, and domestic_intelligence_agencies; the firms' arbitrage-grade exit and the regulators' ability to alternate between defensive and offensive use push them toward the beneficiary end. Victim declarations drive high directionality for transnational_data_subjects (trapped exit places them nearest the full-target end despite negligible power — trap modulation, not power, sets their chi), for small_open_jurisdictions (dual-role nets them well above symmetric: the rhetorical benefit of invocation is outweighed by unenforceable protection), and for eu_data_protection_authorities, who are the bound's direct targets and bear its enforcement-curtailment costs most concentratedly. Directionality_overrides are deliberately omitted: the derivation chain produces accurate values from the declared structure, and the available override granularity (per power atom) would misfire across the three heterogeneous institutional seats (sovereign regulators, intelligence agencies, EU regulators) that share the institutional atom but occupy opposite structural positions. One caveat flagged for downstream readers: domestic_intelligence_agencies benefit defensively — their gain is immunity from scrutiny rather than collected rents — so their derived low d should not be read as rent-capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interstate overreach across borders) remains live — data flows intensified it rather than retiring it — so no mandatrophy resolution is declared and no sunset clause applies. The tangled_rope classification guards against two mislabels in opposite directions: it prevents whitewashing the arrangement as pure rope (naming transnational_data_subjects, small_open_jurisdictions, and eu_data_protection_authorities as payers forces the fragmentation and redress-gap costs into the ledger rather than letting reciprocity rhetoric absorb them), and it prevents condemning it as a snare (the coordination function is genuine — without reciprocal restraint, jurisdictional claims would proliferate into open regulatory conflict — so the extraction is asymmetric cost-bearing riding on real coordination, not coordination-as-cover). The classification also blocks a piton reading: the boundary-maintenance function is actively performed and contested, not inertially retained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading (territorial_sovereignty_reading) of kernel gdpr_article_3_scope; what happens to the beneficiary/victim structure and epsilon if a sibling reading is adopted instead?',
    'Adoption of effects_jurisdiction_reading relocates the protected class to EU residents wherever their data travels, recasts non-EU processors as governed participants rather than targets, and moves epsilon down toward the effects seat''s estimate; adoption of market_access_reading reframes compliance expenditure as consensual market-entry pricing, damping measured extraction further and dissolving the non-consent grievance that anchors the territorial seat. The structural element readings differ on is the legitimacy source of jurisdiction over foreign processors.',
    'Reading choice flips the victim and beneficiary sets and moves epsilon across the coordination/extraction boundary; the three stories must be read together to triangulate the kernel — no single reading''s classification is the kernel''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: one of three sibling readings of the Article 3 scope kernel; sibling adoptions restructure the constraint.').

omega_variable(
    selective_invocation_asymmetry,
    'Great powers invoke the territorial bound defensively (against foreign reach into their firms'' data) while projecting extraterritorially offensively (long-arm sanctions, forced-disclosure statutes reaching data held abroad). Is the operative norm territorial restraint, or power-weighted jurisdictional competition wearing territorial rhetoric?',
    'Code enforcement-relevant jurisdictional events across the interval by invoker power level and by direction (defensive invocation versus offensive projection); test whether invocation frequency tracks interest rather than principle.',
    'If offensive projection dominates, the bound functions as armor for the strong and shackle for the weak — extraction concentrates on small_open_jurisdictions and trapped data subjects, trending the strong-power seats toward snare-like operation while weak seats retain rope-like coordination; if restraint dominates, the tangled_rope reading holds uniformly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_asymmetry, empirical, 'Whether territorial rhetoric masks power-weighted jurisdictional competition.').

omega_variable(
    fragmentation_cost_incidence,
    'Do measurable cross-border redress failures for data subjects actually track the territorial bound''s operation, or are they dominated by platform design and enforcement-resource choices independent of jurisdiction?',
    'Incident-level attribution studies separating cases where jurisdictional refusal was the binding obstacle from cases where identification, detection, or corporate opacity was.',
    'Determines whether transnational_data_subjects is correctly specified as a victim of this constraint and whether the payer side of epsilon is overweighted or underweighted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_cost_incidence, empirical, 'Attribution of the cross-border protection gap to the bound versus confounding causes.').

omega_variable(
    localization_resistance_efficacy,
    'Does data localization actually resist extraterritorial regulatory claims as this reading predicts, or does mirroring and sharding defeat it while entrenching domestic incumbents?',
    'Compare stated policy objectives of localization mandates against measured changes in data-flow topology and in susceptibility to foreign legal process after implementation.',
    'If localization is ineffective, the resistance metric overstates the bound''s contest intensity and part of the suppression_requirement series counts symbolic compliance — flattening the enforcement ratchet the temporal series records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(localization_resistance_efficacy, empirical, 'Whether the predicted resistance mechanism performs as modeled.').

omega_variable(
    hybrid_framework_stability,
    'Can hybrid instruments (adequacy determinations paired with targeting tests) indefinitely mediate between the territorial and effects readings, or does the underlying legitimacy disagreement force eventual resolution in one direction?',
    'Track whether successive adequacy renewals require increasingly effects-shaped conditions, and whether court rulings increasingly justify reach in effects terms rather than territorial-concession terms.',
    'If mediation is unstable, this reading and effects_jurisdiction_reading are on a convergence-or-conflict trajectory rather than a stable coexistence; classification pressure shifts toward whichever reading the hybrids quietly adopt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_framework_stability, conceptual, 'Durability of the hybrid arrangements bridging rival readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_art3_territorial_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gdpr_art3_territorial_tr_t0, observed).
narrative_ontology:measurement(gdpr_art3_territorial_tr_t5, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(gdpr_art3_territorial_tr_t5, observed).
narrative_ontology:measurement(gdpr_art3_territorial_tr_t10, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(gdpr_art3_territorial_tr_t10, observed).
narrative_ontology:measurement(gdpr_art3_territorial_tr_t15, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(gdpr_art3_territorial_tr_t15, observed).
narrative_ontology:measurement(gdpr_art3_territorial_tr_t20, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(gdpr_art3_territorial_tr_t20, observed).
narrative_ontology:measurement(gdpr_art3_territorial_tr_t23, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 23, 0.16).
narrative_ontology:measurement_basis(gdpr_art3_territorial_tr_t23, observed).
narrative_ontology:measurement(gdpr_art3_territorial_tr_t26, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 26, 0.17).
narrative_ontology:measurement_basis(gdpr_art3_territorial_tr_t26, observed).
narrative_ontology:measurement(gdpr_art3_territorial_tr_t30, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(gdpr_art3_territorial_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_art3_territorial_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(gdpr_art3_territorial_be_t0, observed).
narrative_ontology:measurement(gdpr_art3_territorial_be_t5, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement_basis(gdpr_art3_territorial_be_t5, observed).
narrative_ontology:measurement(gdpr_art3_territorial_be_t10, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(gdpr_art3_territorial_be_t10, observed).
narrative_ontology:measurement(gdpr_art3_territorial_be_t15, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 15, 0.19).
narrative_ontology:measurement_basis(gdpr_art3_territorial_be_t15, observed).
narrative_ontology:measurement(gdpr_art3_territorial_be_t20, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(gdpr_art3_territorial_be_t20, observed).
narrative_ontology:measurement(gdpr_art3_territorial_be_t23, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 23, 0.26).
narrative_ontology:measurement_basis(gdpr_art3_territorial_be_t23, observed).
narrative_ontology:measurement(gdpr_art3_territorial_be_t26, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 26, 0.28).
narrative_ontology:measurement_basis(gdpr_art3_territorial_be_t26, observed).
narrative_ontology:measurement(gdpr_art3_territorial_be_t30, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement_basis(gdpr_art3_territorial_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_art3_territorial_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(gdpr_art3_territorial_su_t0, observed).
narrative_ontology:measurement(gdpr_art3_territorial_su_t5, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement_basis(gdpr_art3_territorial_su_t5, observed).
narrative_ontology:measurement(gdpr_art3_territorial_su_t10, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(gdpr_art3_territorial_su_t10, observed).
narrative_ontology:measurement(gdpr_art3_territorial_su_t15, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(gdpr_art3_territorial_su_t15, observed).
narrative_ontology:measurement(gdpr_art3_territorial_su_t20, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(gdpr_art3_territorial_su_t20, observed).
narrative_ontology:measurement(gdpr_art3_territorial_su_t23, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 23, 0.58).
narrative_ontology:measurement_basis(gdpr_art3_territorial_su_t23, observed).
narrative_ontology:measurement(gdpr_art3_territorial_su_t26, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 26, 0.62).
narrative_ontology:measurement_basis(gdpr_art3_territorial_su_t26, observed).
narrative_ontology:measurement(gdpr_art3_territorial_su_t30, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(gdpr_art3_territorial_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, data_localization_mandates).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, us_cloud_act_extraterritorial_reach).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'GDPR's extraterritorial scope' decomposes into three sibling readings of the kernel gdpr_article_3_scope, one story each per the epsilon-invariance principle. The territorial_sovereignty_reading (this file) treats the bound on regulatory authority as the operative constraint and authors epsilon for it in its own lights; effects_jurisdiction_reading instantiates the effects-based extension rule with a different beneficiary/victim structure (protected residents, governed foreign processors); market_access_reading instantiates the conditional-access gate, reframing compliance as consensual market pricing. Epsilon differs across siblings over the shared referent because legitimacy assessment is reading-indexed; the siblings are linked bidirectionally through affects_constraints, and hybrid instruments (adequacy plus targeting tests) mediate between them in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
