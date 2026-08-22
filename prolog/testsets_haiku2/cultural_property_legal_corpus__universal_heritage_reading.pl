% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Doctrine in Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   This constraint instantiates the universal-heritage reading of the
 *   cultural-property legal corpus. It holds that cultural artifacts,
 *   regardless of geographic origin, are humanity's shared heritage and that
 *   legitimate authority to determine their disposition rests with
 *   institutions (museums, universities, conservation bodies) that maximize
 *   preservation and universal access. Under this reading, holding
 *   institutions become primary beneficiaries; successor states' sovereignty
 *   claims are reframed as 'nationalist particularism' that threatens the
 *   global commons; indigenous communities' claims for repatriation are
 *   treated as emotional or ceremonial rather than legally binding. The
 *   constraint extracts from claimant states through legal costs, diplomatic
 *   friction, and the structural foreclosure of repatriation as a legitimate
 *   remedy. This reading is contested: the sovereign-repatriation reading
 *   treats the same legal corpus as vehicle for colonial restitution; the
 *   indigenous-stewardship reading treats it as erasing indigenous authority.
 *   This constraint is authored as ONE reading, not a neutral description.
 *
 * KEY AGENTS:
 *   - major_holding_institutions: Primary beneficiary (museums, universities, foundations in Global North hold ~90% of contested artifacts; control preservation standards and international discourse)
 *   - successor_claimant_states: Primary targets (bear legal costs, diplomatic isolation, identity-harm from being told their claims are 'nationalist threats')
 *   - indigenous_communities: Structurally excluded targets (powerless, trapped, invisible in international legal forums despite being the artifact creators)
 *   - international_legal_bodies: Agenda-setters that enforce the universal-heritage frame (UNESCO, ICJ, ICOMOS consistently favor holding institutions)
 *   - conservation_science_professionals: Secondary beneficiaries (career advancement, research access, prestige tied to major collections)
 *   - observer_postcolonial_scholars: Analytical seat documenting the frame's colonial reproduction (excluded from policy influence)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.71).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.68).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine in Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '871e8390-3b39-4462-8fea-e0725e61790a').
narrative_ontology:cs_kernel_codification('871e8390-3b39-4462-8fea-e0725e61790a', fixed_text).
narrative_ontology:cs_authority_grounding('871e8390-3b39-4462-8fea-e0725e61790a', extraction).
narrative_ontology:cs_interpretation_layer_present('871e8390-3b39-4462-8fea-e0725e61790a').
narrative_ontology:cs_reading_relation('871e8390-3b39-4462-8fea-e0725e61790a', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('871e8390-3b39-4462-8fea-e0725e61790a', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('871e8390-3b39-4462-8fea-e0725e61790a', foundational, universal_heritage_authority).
narrative_ontology:cs_axiom_status(universal_heritage_authority, holdable).
narrative_ontology:cs_axiom_grounding('871e8390-3b39-4462-8fea-e0725e61790a', universal_heritage_authority, instrumental).
narrative_ontology:cs_axiom('871e8390-3b39-4462-8fea-e0725e61790a', foundational, preservation_capacity_justifies_institutional_retention).
narrative_ontology:cs_axiom_status(preservation_capacity_justifies_institutional_retention, holdable).
narrative_ontology:cs_axiom_grounding('871e8390-3b39-4462-8fea-e0725e61790a', preservation_capacity_justifies_institutional_retention, empirically_contingent).
narrative_ontology:cs_reference_frame('871e8390-3b39-4462-8fea-e0725e61790a', universal_stewardship_legitimacy).
narrative_ontology:cs_drift_state('871e8390-3b39-4462-8fea-e0725e61790a', post_sovereign_repatriation_mobilization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('871e8390-3b39-4462-8fea-e0725e61790a', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, universal_access_advocacy_networks).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, successor_claimant_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, universal_access_networks).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, conservation_science_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large Western museums, universities, and heritage foundations that hold globally significant artifacts. Control the international discourse on preservation standards, set exhibition policies, determine conservation methodology. Argue that universal access and scientific study justify retention. Directly benefit from legal frameworks treating them as the legitimate custodians and international arbiters of heritage value. Have resources to defend claims in international courts and UNESCO bodies.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions, beneficiary).

% Nations claiming historical continuity with expropriated peoples and colonial-era source communities. Seek repatriation of artifacts as assertions of sovereignty, historical redress, and cultural identity. Bear high diplomatic and legal costs in pursuit of repatriation claims. Face dismissal of claims as 'particularist' or 'nationalist' threats to the global commons. Cannot credibly exit the international legal framework without sacrificing legitimacy on the world stage.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, successor_claimant_states, payer,
    moderate, generational, identity_locked, national).

% Indigenous peoples whose ancestors created the artifacts now held in Western institutions. Regard the objects as sacred, culturally alive, and integral to ongoing ceremonial and knowledge transmission. Have minimal access to international legal forums. Cannot afford litigation costs. Are structurally excluded from the universal-heritage framework, which treats cultural objects as decontextualized specimens rather than living repositories of communal knowledge.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, payer,
    powerless, civilizational, trapped, local).

% International scholarly networks, conservation organizations, educational institutions, and digital humanities advocates. Benefit from centralized, stable, internationally curated collections. Argue that universal access and comparative study require stable, professionally managed holdings. Have legitimate interest in preservation and scholarly access, though not directly captured by holding institutions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, universal_access_networks, beneficiary,
    organized, biographical, mobile, global).

% People living in regions where artifacts originated but lack formal sovereign claim recognition or institutional representation. Would argue for local contextualization, community-based interpretation, and rotation of artifacts through source regions. Are systematically excluded from the universal-heritage framework's deliberative bodies.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, source_region_populations, excluded,
    organized, generational, constrained, regional).

% UNESCO, International Court of Justice, regional human rights courts, ICOMOS. Adjudicate repatriation claims, set preservation standards, interpret cultural property conventions. Have adopted the universal-heritage framing as the baseline assumption in most major decisions. Structurally favor holding institutions' arguments about preservation and access over claimant states' arguments about sovereignty and indigenous communities' arguments about cultural continuity.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_legal_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Museum conservators, archaeologists, chemists, and technical specialists whose careers are built on accessing and studying centralized collections. Benefit from consolidated holdings and professional standards. Have material interest in the stability of major institutions. Provide scientific legitimacy to the universal-heritage framing ('objects are safer here').
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, conservation_science_professionals, beneficiary,
    organized, biographical, mobile, global).

% Academics, activists, and analysts who document how the universal-heritage doctrine reproduces colonial asymmetries. Can publish critiques but have limited influence on institutional policy or international legal outcomes. Provide the alternative framings but operate from outside the benefiting coalition.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, observer_postcolonial_scholars, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, globally recognized standard for artifact preservation, scientific access, and comparative study: artifacts held in accredited institutions are protected by international norms, professionally conserved, and available to international scholarship. Solves the problem of fragments scattered across multiple contexts with varying preservation capacity.
% TRANSFER_FUNCTION: Transfers cultural authority from source communities and successor states to internationally recognized holding institutions (museums, universities, conservation bodies). Transfers legitimacy claims about what 'proper' preservation looks like from local/communal practices to Western scientific conservation standards. Transfers symbolic capital and prestige from the nation or community to the holding institution.
% ABSENT_VOICES: Indigenous communities whose ancestors created the artifacts are structurally excluded from the international legal forums where repatriation is debated. Source-region populations lack institutional representation in UNESCO bodies and conservation councils. Successor states lack the legal-historical standing to make claims if they cannot establish unbroken continuity with the expropriated group — a requirement the universal-heritage framework itself establishes.
% DISAPPEARANCE_RATIONALE: If the universal-heritage doctrine and its enforcement disappeared, the architecture of international cultural property law would collapse. Holding institutions would face immediate, successful repatriation claims. Successor states and indigenous communities would regain authority to define what happens to the artifacts. Scientific access would require negotiation rather than default assumption. The international museum system would reorganize around bilateral agreements, partnership models, and distributed holdings rather than centralized Western collections.
% FOUNDING_PROBLEM: Artifacts from colonized regions were expropriated and scattered across private collections and minor institutions with inconsistent preservation practices. Scholars could not access comparative collections. Political instability, war, and looting threatened objects in their source regions. Western museums offered professional conservation, stable storage, and international scholarly access.
% FOUNDING_PROBLEM_CORROBORATION: Holding institutions and conservation organizations affirm the founding problem remains live, citing ongoing conflict in source regions and limited preservation capacity outside major Western institutions. Successor states, indigenous communities, and postcolonial scholars attest the founding problem was instrumentally solved but the solution was weaponized to justify permanent retention rather than transitional preservation. Legislative testimony from claimant-state diplomats and anthropological analysis from outside benefiting institutions support the contested status.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.71 over the interval as legal challenges accumulate (more repatriation claims → higher enforcement costs), yet the doctrine becomes MORE entrenched through repeated international court rulings. The suppression trajectory (0.52→0.68) models increasing institutional effort to defend the frame against mounting resistance. Theater rises to 0.44 by interval end, reflecting that 'preservation' and 'universal access' increasingly perform the function of legitimating retention rather than solving the founding problem. The constraint is CLAIMED as rope (genuine coordination in access) while metrics describe substantially extractive, actively suppressed operation — this gap is the analytical point the engine measures. The founding problem (conservation capacity) was real in 1920 but is increasingly dead (claimant states now have professional conservation capacity); the reading's persistence despite problem death is the marker of extraction riding on a vestigial coordination function. Measurements share one time grid across all three metrics per the alignment rule; every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the holding institution's seat, the arrangement is rope: genuine coordination (stable access, professional curation, comparative study) with justifiable beneficiary return (prestige, research access). From the claimant-state seat, the same arrangement is snare: coercive retention of what was stolen, dressed in universal-access language, enforced by international legal bodies controlled by the same institutions holding the artifacts. From the indigenous-community seat, it is snare: cultural erasure and civilizational harm, with no remedial voice. The engine computes these divergent types from the structural data (power, exit, beneficiary/victim status); the reading's coherence depends on all three seats being present and their asymmetries being declared.
 *
 * DIRECTIONALITY LOGIC:
 *   Holding institutions occupy the beneficiary end of the directionality scale (d ≈ 0.05–0.15): they set the rules, collect prestige and research access, have arbitrage-grade exit (can reposition to new source materials if one stream closes). Successor claimant states occupy the high-target end (d ≈ 0.75–0.85): they are identity-locked (cannot exit without sacrificing sovereignty legitimacy), face legal foreclosure of remedies (the doctrine itself defines their claims as illegitimate), and bear the costs (diplomatic friction, legal fees, cultural harm from non-recognition). Indigenous communities sit at the trapped end (d ≈ 0.90–0.95): powerless, excluded from forums, with no exit short of cessation of indigenous identity. The universal-access networks and conservation professionals sit near symmetric (d ≈ 0.40–0.50): they genuinely benefit from centralized collections (coordination dividend) but also collect prestige and research opportunities (extraction component). Divergence in d values across seats drives the per-seat classification divergence the engine computes.
 *
 * MANDATROPHY ANALYSIS:
 *   The universal-heritage reading exhibits clear mandatrophy: the founding problem (conservation capacity scattered, endangered) was solved by ~1980 — successor states now have professional institutions, digital documentation enables distributed access, and major climate-risk regions have improved infrastructure. Yet the doctrine persists and grows more institutionally entrenched (UNESCO conventions multiply, international courts reaffirm holdings, possession has lengthened into presumptive right). The theater ratio climbing to 0.44 models this drift: enforcement machinery increasingly defends the rule rather than solves the problem. The divergence between founding_problem_status=contested and theater_ratio=0.44 is the diagnostic signature of mandatrophy resolved in favor of extraction — the founding problem is officially 'contested' (neutrality performance) while enforcement activity is predominantly theatrical (defending presumption rather than solving original problem). The underlying extraction is high (0.71) precisely because the mandate is dead and the structure persists by pure institutional inertia and power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_death_vs_persistence,
    'Is the founding problem (conservation capacity scattered and endangered) genuinely dead, or does it persist in regions the universal-heritage framework treats as peripheral?',
    'Global audit of conservation capacity in source regions, conflict-affected areas, and Global South institutions. Compare preservation outcomes for artifacts held in Western institutions vs. those returned to claimant states or left in source regions (20+ year follow-up).',
    'If the founding problem is dead in high-profile cases but alive only in peripheral regions, the doctrine becomes a mechanism for retaining high-value artifacts while appearing responsive (selective application of the mandate, classic mandatrophy). If the problem is structurally dead, reclassify to snare (pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_death_vs_persistence, empirical, 'Whether the constraint''s founding justification remains live or has atrophied.').

omega_variable(
    preservation_outcome_divergence,
    'Are objects held in Western institutions materially better preserved than those returned to claimant states, or does the universal-heritage framing exaggerate Western preservation advantages to justify retention?',
    'Comparative conservation science: measure decay rates, environmental conditions, treatment outcomes for matched artifact sets in Western museums vs. repatriated collections over 30+ year intervals. Control for artifact fragility and climate.',
    'If outcomes are equivalent or superior in source regions, the preservation justification is false and the constraint reclassifies to snare. If Western institutions materially out-perform, the rope framing gains structural support (coordination dividend is real). If outcomes diverge by artifact type or region, the universal-heritage reading over-generalizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_outcome_divergence, empirical, 'Whether the constraint delivers on its preservation claim or uses preservation language to justify retention.').

omega_variable(
    reading_kernel_foreclosure,
    'Can the universal-heritage reading and the indigenous-stewardship reading coexist within a single legal framework, or do they have logically incompatible core premises about what counts as legitimate authority?',
    'Test case: a state ratifies both a universal-heritage treaty (UNESCO) and an indigenous-rights treaty (ILO 169) and is asked to arbitrate a repatriation claim from an indigenous community. Which framework governs? If one deterministically overrides the other, they foreclose; if the state must choose by political negotiation, they coexist.',
    'If foreclosure is found (engine computation), the reading''s relation to indigenous-stewardship should be reclassified from coexists_with to forecloses. If coexistence holds, the classification stands. Foreclosure would suggest the universal-heritage reading is more hegemonic than structures of international law formally admit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether the universal-heritage reading''s core premise about legitimate authority logically precludes indigenous stewardship claims.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.68) structural (legal barriers, resource asymmetry, institutional closure) or internalized (claimant states have accepted the universal-heritage framing as legitimate)?',
    'Post-exit suppression trajectory: if a claimant state successfully repatriates artifacts and their behavior toward the remaining constraints changes (confidence, new claims), suppression was structural. If they continue deferring to Western expertise even after successful repatriation, it is partially internalized (the reading has colonized their self-perception).',
    'If suppression is internalized, the measured value understates the effective suppression (the target carries the frame with them). If structural, the measured value is accurate. This affects prognosis: internalized suppression is harder to break through repatriation alone; structural suppression would shift rapidly once legal barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of repatriation claims is external or has colonized claimant-state self-perception.').

omega_variable(
    reading_authority_capture,
    'Is the universal-heritage reading a genuine doctrine about shared human heritage, or does it function primarily as cover for institutional capture of international cultural-property governance by the same institutions that hold the artifacts?',
    'Voting analysis in UNESCO and conservation bodies: do holding institutions and their allied states systematically block repatriation proposals? Do international legal bodies show systematic bias toward holdings over claims? Historical analysis: did the universal-heritage doctrine emerge from independent scholarly consensus or from institutional advocacy?',
    'If capture is found, the constraint reclassifies from rope (coordination with captured rents) to snare (pure extraction dressed as coordination). If the doctrine is genuinely independent of institutional interest, rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authority_capture, empirical, 'Whether the universal-heritage reading represents genuine scholarly consensus or institutional capture disguised as doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cult_tr_t5, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(cult_tr_t15, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(cult_tr_t25, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(cult_tr_t35, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 35, 0.44).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cult_be_t5, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cult_be_t15, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(cult_be_t25, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(cult_be_t35, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 35, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cult_su_t5, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(cult_su_t10, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(cult_su_t15, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(cult_su_t20, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(cult_su_t25, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cult_su_t35, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 35, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.18).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% The cultural_property_legal_corpus is a contested kernel with three structurally distinct readings. Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different classifications. This constraint (universal_heritage_reading) models the doctrine that holds institutional possession as legitimate and treats claimant sovereignty as particularist threat (high ε extraction from claimants). The sovereign_repatriation_reading models the framework treating repatriation as restitution for colonial expropriation (moderate ε extraction from holders). The indigenous_stewardship_reading models the framework treating artifacts as sacred communal property (high ε extraction from external holders/museums). Each reading has its own constraint_id and is a separate story; they are linked via network.affects_constraints because shifts in one reading's legitimacy directly affect the others' institutional standing. The ε-invariance principle requires decomposition: a single 'cultural property law' constraint with measurement-basis parameters would fabricate artificial neutrality where none exists. The readings are incommensurable; they must be separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
