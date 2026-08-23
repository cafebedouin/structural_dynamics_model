% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundary Norms as Continuously Renegotiated Local Practice
 *   domain: social_anthropology/political_economy
 *
 * SUMMARY:
 *   Across rural and small-town South Asia, hereditary status groups called
 *   jatis organize marriage matching, hereditary work allocation, credit and
 *   mutual aid, ritual certification, and dispute settlement through
 *   thousands of local councils and kin networks rather than any central
 *   registry. This story instantiates the localized_practice_reading of the
 *   jati kernel: boundaries are coordination norms subject to continuous
 *   local renegotiation and proliferation — groups split and fuse, rename
 *   themselves, bid for higher precedence across generations, and absorb
 *   defectors case by case — with enforcement weak and municipal in scale.
 *   Empirical proliferation to 3000+ recorded names is this reading's
 *   signature datum. The epsilon referent is the standing arrangement under
 *   contest (the lived jati order across the interval), assessed strictly by
 *   this reading's own lights: genuine coordination dominates, coercion is
 *   real but thin and decaying, and the costs the arrangement imposes are
 *   treated as negotiable local texture rather than systematic victimization.
 *   The colloquial label 'the caste system' decomposes into three
 *   structurally distinct claims — doctrinal fixity, administrative
 *   reification, lived local practice — authored as separate linked files
 *   (see network.dual_formulation_note); this file is the third. Claim and
 *   metrics are authored independently: the reading claims rope; the metrics
 *   describe what the reading honestly observes, including costs it does not
 *   deny.
 *
 * KEY AGENTS:
 *   - - village_dominant_peasant_castes: Primary beneficiary and local agenda-setter (organized/constrained) — captures deference, precedence, and adjudication authority; would forfeit positional capital if the rank order dissolved
 *   - - merchant_trading_jatis: Distributed beneficiary (organized/mobile) — monetizes intra-group trust across regional diasporas; exits by relocation without losing the network
 *   - - hereditary_artisans_service_jatis: Dual-positioned participant (moderate/constrained) — nets guild allocation and mutual-aid gains against deference burdens renegotiated village by village
 *   - - caste_panchayat_elders: Agenda-setter (organized/identity_locked) — authority constituted by boundary adjudication itself; interprets norms case by case to absorb change
 *   - - priestly_ritual_lineages: Service beneficiary (moderate/identity_locked) — certifies status claims through life-cycle rites for fees; supplies legitimating narratives on demand
 *   - - sanskritizing_status_claimants: Mobile claimant (organized/mobile) — exploits boundary negotiability for multi-generational status ascent
 *   - - boundary_crossing_individuals: Cost-bearing seat (powerless/trapped) — pays sanction prices individually; their cases drive much of the renegotiation
 *   - - women_in_endogamous_marriage_markets: Concentrated cost-bearing seat (powerless/trapped) — marriageability is the principal currency in which boundaries are maintained
 *   - - unattached_newcomer_migrants: Excluded outsider (moderate/mobile) — outside the coordination web entirely; would contest its closure if seated
 *   - - ethnographic_fieldworkers: Analytical observer (analytical/analytical) — documents name variance, fission events, and council proceedings; produces the proliferation evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.34).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.31).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.27).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.46).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundary Norms as Continuously Renegotiated Local Practice").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1').
narrative_ontology:cs_kernel_codification('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', distributed).
narrative_ontology:cs_authority_grounding('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', practice).
narrative_ontology:cs_interpretation_layer_present('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1').
narrative_ontology:cs_reading_relation('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', foundational, local_negotiation_constitutes_jati_boundaries).
narrative_ontology:cs_axiom_status(local_negotiation_constitutes_jati_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', local_negotiation_constitutes_jati_boundaries, empirically_contingent).
narrative_ontology:cs_axiom('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', foundational, category_proliferation_indicates_weak_enforcement).
narrative_ontology:cs_axiom_status(category_proliferation_indicates_weak_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', category_proliferation_indicates_weak_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', continuously_negotiated_local_consensus).
narrative_ontology:cs_drift_state('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', contemporary_post_enumeration_urbanization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b56cdf6-c12d-4aa6-a4d3-33d0b49125d1', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, village_dominant_peasant_castes).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, merchant_trading_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, hereditary_artisans_service_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, priestly_ritual_lineages).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, sanskritizing_status_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, hereditary_artisans_service_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, boundary_crossing_individuals).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, women_in_endogamous_marriage_markets).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, barthian_boundary_maintenance).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, sanskritization_mobility_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the larger share of village land, set irrigation turns and labor arrangements, and preside over precedence questions at weddings, funerals, and festivals. Receive deference and first claim on communal resources. Maintain strict in-group marriage to keep alliance networks concentrated. If the local rank order dissolved, their positional standing and brokerage income would go with it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, village_dominant_peasant_castes, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, village_dominant_peasant_castes, agenda_setter).

% Run credit, commodity, and money-transfer networks built on intra-group trust and reputational legibility. Endogamous marriage keeps commercial knowledge and alliance capital inside the circle; shared festival calendars and council structures settle disputes internally. Branch communities across regions replicate the same template, so relocating rarely costs them the network.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, merchant_trading_jatis, beneficiary,
    organized, generational, mobile, continental).

% Supply carpentry, metalwork, weaving, laundering, and similar services through hereditary household-to-patron assignments, receiving grain shares, festival payments, and mutual aid in return. Bear ritual-distance rules and deference expectations imposed by higher-ranking neighbors, renegotiated household by household and village by village. Switching trades or patrons is possible but severs the aid and work-allocation web.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, hereditary_artisans_service_jatis, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, hereditary_artisans_service_jatis, beneficiary).

% Convene assemblies to hear marriage disputes, commensality complaints, and precedence claims; levy fines, prescribe penances, and broker reconciliations. Their authority exists only because boundary questions keep arising; interpreting the norms case by case is how they absorb change without conceding that the rules themselves shifted.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, caste_panchayat_elders, agenda_setter,
    organized, generational, identity_locked, local).

% Perform the life-cycle rites through which households certify standing — weddings, purifications, funerary observances — collecting fees and offerings. Supply legitimating genealogies and custom precedents on request, serving whichever local settlement prevails rather than imposing a single doctrinal standard.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, priestly_ritual_lineages, beneficiary,
    moderate, generational, identity_locked, regional).

% Adopt the dress, diet, ritual repertoire, and origin stories of higher-ranking groups, commission genealogies, and petition councils for precedence recognition. Mobility bids typically take two to three generations and succeed unevenly; the possibility itself depends on the boundaries staying negotiable.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, sanskritizing_status_claimants, beneficiary,
    organized, generational, mobile, regional).

% Marry, eat, or work across jati lines. Outcomes vary sharply by locality: some councils fine and briefly shun them, some expel them outright, some quietly register the union as precedent. Individually exposed, dependent on kin and community for livelihood and protection, with no collective bargaining position of their own.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, boundary_crossing_individuals, payer,
    powerless, biographical, trapped, local).

% Their marriageability is the principal currency in which group boundaries are maintained: hypergamy rules, dowry negotiations, and family-honor policing concentrate compliance demands on them. Consent is mediated by senior kin, and refusal carries heavy personal cost.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, women_in_endogamous_marriage_markets, payer,
    powerless, biographical, trapped, regional).

% Arrive in towns and cities without jati sponsorship, cut off from the credit, job-referral, and marriage-brokerage webs locals take for granted. Some affiliate with a host group over years; others remain permanently peripheral, eligible for nobody's mutual aid.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, unattached_newcomer_migrants, excluded,
    moderate, biographical, mobile, national).

% Document name variance between neighboring valleys, record council proceedings, trace fission and fusion events, and compile the proliferation counts on which this reading's evidence rests. Positioned outside the normative order, they neither collect nor pay.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, ethnographic_fieldworkers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, village_dominant_peasant_castes).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches marriage alliances inside kin-defined groups, allocates hereditary craft and service work between households and patrons, pools credit and mutual aid along trusted ties, and arbitrates local precedence disputes — keeping transaction partners mutually legible without any central registry.
% TRANSFER_FUNCTION: Moves deference, ritual precedence, and adjudication authority upward within village rank orders toward dominant landed castes; moves marriage rights, mutual aid, credit access, and work allocation inward within each group; moves dispute-settlement labor onto elder councils.
% ABSENT_VOICES: Newcomers without jati sponsorship, individuals fined or expelled for boundary crossings, and the lowest-ranked service groups whose deference obligations are treated as background conditions are rarely seated in the council deliberations where boundaries get renegotiated; their objections typically arrive as cases to be adjudicated rather than as standing voices in the rule-making itself.
% DISAPPEARANCE_RATIONALE: Marriage matching, hereditary work allocation, intra-group credit, and local dispute arbitration would lose their organizing rails overnight; kin networks would fragment into narrower lineages, trading diasporas would have to rebuild trust structures from scratch, and village labor, water-sharing, and festival arrangements would need wholly new governance within months.
% FOUNDING_PROBLEM: Settled agrarian society had to organize cooperation among strangers at village scale — deciding who may marry whom, who works for whom, who shares water and hearth, and who settles disputes — in an environment with thin state administration and no impersonal institutions for generating trust.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of pre-modern South Asia and comparative institutional economists corroborate the trust-and-coordination genealogy from outside the benefiting parties; Ambedkarite and Dalit scholarship corroborates from the same outside position that the arrangement persisted long past any functional necessity while disputing that its origins were benignly functional — attesting jointly that the founding problem is partly superseded rather than simply solved.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.34: the arrangement's costs are real (deference obligations, ritual-distance rules, constrained marriage choice) but diffuse, negotiable, and reciprocally embedded in aid and matching benefits; no seat captures monetary rent — the captured good is positional (precedence, adjudication authority), which is why gain_flow names the dominant-caste seat while epsilon stays moderate-low. Suppression 0.31 reflects enforcement that is genuine but municipal-scale — councils, kin pressure, occasional fines and ostracism — with no central apparatus; alternatives (migration, occupational switching, group fission, renaming) remain available at the group level, hence accessibility_collapse 0.27, far below the natural-law range. Resistance 0.46: constant micro-defiance that the system metabolizes as renegotiation rather than crushing. Theater 0.14: nearly all council activity adjudicates live cases; little is performed. The measurement series share one seven-point grid (1891–2023) so every metric is asserted at every examined time point; trajectories are monotonic with no oscillation, so no intermittent-reinforcement concern arises. Suppression_requirement is tracked because this story's central dynamic IS enforcement-capacity decay — constitutional equal-rights provisions, urban anonymity, and formal labor, credit, and marriage markets absorbing the arrangement's functions — not merely shifting extraction; endpoint values equal the base_properties scalars. Receipt surface: deference and adjudication authority demonstrably accrue to the dominant-caste seat, so gain_flow names it rather than 'diffuse'; for the seats that could relax the boundaries (elders and dominant castes), doing so would forfeit their positional capital wholesale, so fixing_cost is prohibitive — both are situation facts, not classifications.
 *
 * PERSPECTIVAL GAP:
 *   From the dominant-caste and elder seats the same norms read as an inherited commons they steward; from the boundary-crosser and marriage-market seats the identical norms read as sanctions aimed personally at them; the artisan and service groups straddle both, collecting guild and aid benefits while bearing deference burdens. The engine computes these per-seat divergences from power, horizon, exit, and role data; the authored rope claim does not adjudicate them, and nothing in this file forces convergence. An elder computing this arrangement as ordinary coordination and a boundary-crosser computing it as targeted sanction would both be correct from their seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Five beneficiary declarations drive low d (toward subsidy) for those seats: dominant peasant castes, merchant trading jatis, artisan-service groups, priestly lineages, and sanskritizing claimants all collect coordination goods from the same structure that binds them. Cost-bearing appears only on the stakeholder surface (payers: boundary-crossing individuals and women in the marriage markets) because this reading locates boundary costs in negotiable local texture rather than systematic victimization — the victims array is deliberately absent as a reading-indexed authored fact, not an omission: the orthodox and colonial siblings, assessing the same referent by their own lights, author victims, and this file does not. Village-scale verification of compliance is cheap, which dampens the scope amplification of effective extraction. The excluded newcomer seat sits outside the derivation entirely — the arrangement neither subsidizes nor taxes them; it simply does not reach them, which is its own structural fact.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming rope here prevents the opposite error the sibling readings invite: reading a decentralized coordination web as pure extraction merely because its outputs include deference hierarchies. The orthogonal safeguard is temporal: the enforcement-decay series and the omegas keep the claim honest — if intermarriage stays flat and councils keep sanctioning, the rope verdict degrades and the family comparison will expose it. The founding problem is authored contested rather than dead: the original problems (trust among strangers, marriage matching, dispute arbitration) still exist and the arrangement still partially serves them, so the dead-status x world_rearranges mismatch flag does not fire. But the mandate has visibly narrowed; if formal institutions finish absorbing its functions while councils continue convening over heritage and precedence, the remnant risks piton drift — the slow theater_ratio rise across the interval is the early-warning signature, and the rising series is authored rather than smoothed away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which of the three declared readings of the jati_practice_norm kernel best fits the standing arrangement: doctrinal fixity (orthodox_textual_reading), administrative stabilization (colonial_census_reading), or locally renegotiated coordination norms (this file)?',
    'Compile and classify the two sibling files independently, then compare computed types, enforcement profiles, and epsilon values across the linked family; the divergence pattern localizes where the kernel contest actually bites.',
    'If the colonial reading computes strong stabilization and this reading computes thin enforcement, the verdict here describes only the residual local layer of a hybrid arrangement; if all three converge, the kernel contest is nominal and the family collapses toward one classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Indexical uncertainty over which reading of the jati kernel the standing arrangement instantiates.').

omega_variable(
    proliferation_signal_direction,
    'Does the empirical proliferation to 3000+ recorded jati names indicate weak enforcement of boundaries, or unusually granular and therefore tighter social control?',
    'Compare naming volatility and boundary-violation sanction rates before and after systematic enumeration; if names multiplied fastest where sanction capacity was weakest, proliferation tracks enforcement weakness rather than surveillance granularity.',
    'The weak-enforcement reading supports this file''s rope verdict and axiom set; the granular-control reading supports reclassification toward the tangled-rope or snare range via the colonial sibling''s stabilization mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_signal_direction, conceptual, 'Whether category proliferation evidences enforcement weakness or fine-grained control success.').

omega_variable(
    suppression_internalization_split,
    'Is the measured suppression in jati boundary maintenance structural (council sanction, kin pressure, economic dependence on the group web) or internalized (socialized acceptance of rank order and endogamous duty that persists when external sanction is removed)?',
    'Post-exit suppression trajectory: track marriage and commensal behavior among migrants who left jati-dense environments; if boundary norms still govern their choices once council and kin sanction become unavailable, the internalized share is large.',
    'A large internalized share raises effective suppression above the structural measure and pulls per-seat classifications toward the coercive types even where visible local enforcement has decayed; a small share confirms the decay series as the true enforcement picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized mechanism behind the measured suppression scalar.').

omega_variable(
    intermarriage_trajectory,
    'What fraction of new marriages cross jati lines, and is that fraction rising fast enough to dissolve boundary enforcement within a few generations?',
    'Cohort-disaggregated time series from marriage registrations, matrimonial listings, and demographic surveys, separated by region and urban/rural setting.',
    'A steeply rising intermarriage rate strengthens this reading''s rope verdict and dates the arrangement''s transition; a flat, low rate indicates enforcement persisting beneath the fluidity claim and degrades rope certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermarriage_trajectory, empirical, 'Whether boundary enforcement is dissolving demographically or persisting covertly.').

omega_variable(
    epsilon_period_weighting,
    'The standing arrangement under contest spans pre-enumeration village rank orders through enumerated, legally scheduled, electorally mobilized categories; which period anchors epsilon for this reading?',
    'An explicit period-weighting protocol in cross-family meta-analysis, with sensitivity recomputation of epsilon over pre-1951 and post-1991 windows.',
    'Pre-modern weighting raises epsilon toward 0.45 (denser deference economies, stronger council sanction); contemporary weighting lowers it toward 0.25; the classification stays in the rope range across the spread but seat divergences shift materially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epsilon_period_weighting, conceptual, 'Period-weighting ambiguity inside the fixed epsilon referent for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 1891, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_localized_practice_tr_t1891, jati_practice_norm__localized_practice_reading, theater_ratio, 1891, 0.08).
narrative_ontology:measurement_basis(jati_localized_practice_tr_t1891, observed).
narrative_ontology:measurement(jati_localized_practice_tr_t1921, jati_practice_norm__localized_practice_reading, theater_ratio, 1921, 0.09).
narrative_ontology:measurement_basis(jati_localized_practice_tr_t1921, observed).
narrative_ontology:measurement(jati_localized_practice_tr_t1951, jati_practice_norm__localized_practice_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement_basis(jati_localized_practice_tr_t1951, observed).
narrative_ontology:measurement(jati_localized_practice_tr_t1971, jati_practice_norm__localized_practice_reading, theater_ratio, 1971, 0.11).
narrative_ontology:measurement_basis(jati_localized_practice_tr_t1971, observed).
narrative_ontology:measurement(jati_localized_practice_tr_t1991, jati_practice_norm__localized_practice_reading, theater_ratio, 1991, 0.12).
narrative_ontology:measurement_basis(jati_localized_practice_tr_t1991, observed).
narrative_ontology:measurement(jati_localized_practice_tr_t2011, jati_practice_norm__localized_practice_reading, theater_ratio, 2011, 0.13).
narrative_ontology:measurement_basis(jati_localized_practice_tr_t2011, observed).
narrative_ontology:measurement(jati_localized_practice_tr_t2023, jati_practice_norm__localized_practice_reading, theater_ratio, 2023, 0.14).
narrative_ontology:measurement_basis(jati_localized_practice_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(jati_localized_practice_be_t1891, jati_practice_norm__localized_practice_reading, base_extractiveness, 1891, 0.46).
narrative_ontology:measurement_basis(jati_localized_practice_be_t1891, observed).
narrative_ontology:measurement(jati_localized_practice_be_t1921, jati_practice_norm__localized_practice_reading, base_extractiveness, 1921, 0.44).
narrative_ontology:measurement_basis(jati_localized_practice_be_t1921, observed).
narrative_ontology:measurement(jati_localized_practice_be_t1951, jati_practice_norm__localized_practice_reading, base_extractiveness, 1951, 0.41).
narrative_ontology:measurement_basis(jati_localized_practice_be_t1951, observed).
narrative_ontology:measurement(jati_localized_practice_be_t1971, jati_practice_norm__localized_practice_reading, base_extractiveness, 1971, 0.39).
narrative_ontology:measurement_basis(jati_localized_practice_be_t1971, observed).
narrative_ontology:measurement(jati_localized_practice_be_t1991, jati_practice_norm__localized_practice_reading, base_extractiveness, 1991, 0.37).
narrative_ontology:measurement_basis(jati_localized_practice_be_t1991, observed).
narrative_ontology:measurement(jati_localized_practice_be_t2011, jati_practice_norm__localized_practice_reading, base_extractiveness, 2011, 0.35).
narrative_ontology:measurement_basis(jati_localized_practice_be_t2011, observed).
narrative_ontology:measurement(jati_localized_practice_be_t2023, jati_practice_norm__localized_practice_reading, base_extractiveness, 2023, 0.34).
narrative_ontology:measurement_basis(jati_localized_practice_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_localized_practice_su_t1891, jati_practice_norm__localized_practice_reading, suppression_requirement, 1891, 0.54).
narrative_ontology:measurement_basis(jati_localized_practice_su_t1891, observed).
narrative_ontology:measurement(jati_localized_practice_su_t1921, jati_practice_norm__localized_practice_reading, suppression_requirement, 1921, 0.5).
narrative_ontology:measurement_basis(jati_localized_practice_su_t1921, observed).
narrative_ontology:measurement(jati_localized_practice_su_t1951, jati_practice_norm__localized_practice_reading, suppression_requirement, 1951, 0.45).
narrative_ontology:measurement_basis(jati_localized_practice_su_t1951, observed).
narrative_ontology:measurement(jati_localized_practice_su_t1971, jati_practice_norm__localized_practice_reading, suppression_requirement, 1971, 0.4).
narrative_ontology:measurement_basis(jati_localized_practice_su_t1971, observed).
narrative_ontology:measurement(jati_localized_practice_su_t1991, jati_practice_norm__localized_practice_reading, suppression_requirement, 1991, 0.36).
narrative_ontology:measurement_basis(jati_localized_practice_su_t1991, observed).
narrative_ontology:measurement(jati_localized_practice_su_t2011, jati_practice_norm__localized_practice_reading, suppression_requirement, 2011, 0.33).
narrative_ontology:measurement_basis(jati_localized_practice_su_t2011, observed).
narrative_ontology:measurement(jati_localized_practice_su_t2023, jati_practice_norm__localized_practice_reading, suppression_requirement, 2023, 0.31).
narrative_ontology:measurement_basis(jati_localized_practice_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'the jati system / caste system' conflates three structurally distinct constraints, decomposed per the epsilon-invariance principle: (1) jati_practice_norm__orthodox_textual_reading — boundaries fixed by scriptural varna doctrine, deviation sanctioned as pollution (high enforcement, high extraction, doctrinal authority); (2) jati_practice_norm__colonial_census_reading — categories stabilized and reified by external administrative enumeration for governance legibility (upstream of this file: enumeration reshaped the field of names local practice operates on); (3) jati_practice_norm__localized_practice_reading (this file) — boundaries as locally negotiated coordination norms (thin, decaying enforcement, low-moderate extraction, proliferation as signature evidence). Each story carries its own epsilon, beneficiaries, and enforcement profile; they are linked here as a constraint family. Upstream-to-downstream: census enumeration and doctrinal vocabularies both feed the raw material this reading observes being continuously renegotiated, and this file's proliferation evidence in turn pressures the census sibling's stabilization claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
